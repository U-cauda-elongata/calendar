#!/usr/bin/env ruby
# frozen_string_literal: true

require 'fileutils'
require 'json'
require 'net/http'
require 'rexml/document'
require 'rexml/xpath'
require 'time'
require 'uri'

NS = {
  'a' => 'http://www.w3.org/2005/Atom',
  'm' => 'http://search.yahoo.com/mrss/',
  'y' => 'http://www.youtube.com/xml/schemas/2015',
}

class Feed
  attr_reader :title, :entries

  def self.parse(string)
    doc = REXML::Document.new(string)

    alternate = REXML::XPath.first(doc, '/a:feed/a:link[@rel="alternate"]/@href', NS).value
    title = REXML::XPath.first(doc, '/a:feed/a:title/text()', NS).value
    entries = REXML::XPath.match(doc, '/a:feed/a:entry', NS).map do |entry|
      id = REXML::XPath.first(entry, './y:videoId/text()', NS).value
      group = REXML::XPath.first(entry, './m:group', NS)
      [id, {
        name: REXML::XPath.first(entry, './a:title/text()', NS).value,
        time: Time.iso8601(REXML::XPath.first(entry, './a:published/text()', NS).value).to_i,
        link: "https://www.youtube.com/watch?v=#{id}",
        thumbnail: "https://img.youtube.com/vi/#{id}/mqdefault.jpg",
        description: REXML::XPath.first(group, './m:description/text()', NS).value,
      }]
    end.to_h

    new(alternate, title, entries)
  end

  def initialize(alternate, title, entries)
    @alternate = alternate
    @title = title
    @entries = entries
  end

  def to_json(*args)
    {
      'alternate' => @alternate,
      'title' => @title,
      'entries' => @entries.map do |id, entry|
        entry = entry.clone
        entry['id'] = "yt:video:#{id}"
        entry
      end,
    }.to_json(*args)
  end
end

def log_http_response(res)
  STDERR.puts "< HTTP/#{res.http_version} #{res.code} #{res.message}"
end


channels = ARGV

if STDIN.isatty
  system 'ssty', '-echo'
  begin
    STDERR.puts 'API key: '
    key = STDIN.gets.chomp
  ensure
    system 'stty', 'echo'
  end
else
  key = STDIN.gets.chomp
end

cached = channels.each_with_object({}) do |channel, feeds|
  feeds[channel] = begin
    open("data/#{channel}.json") do |f|
      feed = JSON.load(f)
      feed['entries'].filter_map do |entry|
        id = entry.delete('id')
        if id and entry['duration'] # Only videos or ended livestreams have duration.
          id.delete_prefix!('yt:video:')
          [id, entry]
        end
      end.to_h
    end
  rescue Errno::ENOENT
    {}
  end
end

FileUtils.mkdir_p('data')
feed_changed = false
http = Net::HTTP.new('www.youtube.com', Net::HTTP.https_default_port)
http.use_ssl = true
feeds = http.start do
  channels.map do |channel|
    path = "/feeds/videos.xml?channel_id=#{channel}"
    STDERR.puts "> GET https://#{http.address}#{path}"
    res = http.get(path, {
      'Accept' => 'application/atom+xml',
    })
    log_http_response(res)
    res.value
    [channel, res.body]
  end
end.map do |(channel, xml)|
  cache = "data/#{channel.xml}"
  # Check if the cache has updated by its content because the feed URL doesn't return `ETag` nor
  # `Last-modified`.
  feed_changed ||= begin
    open(cache, &:read) != xml
  rescue Errno::ENOENT
    true
  end
  IO.write(cache, xml, mode: 'wb')
  [channel, Feed.parse(xml)]
end.to_h

ids = if feed_changed
  feeds.flat_map {|_, feed| feed.entries.keys } - cached.flat_map {|_, entries| entries.keys }
else
  STDERR.puts 'No feed has changed, skipping API request'
  []
end
items = if ids.empty?
  []
else
  uri = URI('https://youtube.googleapis.com/youtube/v3/videos')
  http = Net::HTTP.new(uri.host, uri.port)
  http.use_ssl = uri.scheme == 'https'
  http.start do
    ids.each_slice(50).with_object([]) do |slice, acc|
      uri.query = URI.encode_www_form([
        ['part', 'contentDetails,liveStreamingDetails'],
        ['fields', 'items(id,contentDetails/duration,liveStreamingDetails(actualStartTime,scheduledStartTime))'],
        ['id', slice.join(',')],
        ['key', key],
      ])
      STDERR.puts "> GET #{uri.to_s.gsub(URI.encode_www_form_component(key), '[API KEY]')}"
      res = http.get(uri.request_uri, {
        'Accept' => 'application/json',
      })
      log_http_response(res)
      res.value
      json = JSON.parse(res.body)
      acc.concat(json['items'])
    end
  end
end

# Include cached entries in the output.
feeds.each do |channel, feed|
  feed.entries.merge!(cached[channel])
end

entries = feeds.each_with_object({}) do |(_, feed), acc|
  acc.merge!(feed.entries)
end

items.each do |video|
  entry = entries[video['id']]
  if video['contentDetails']&.[]('duration')&.match(/^P(?:(\d+)D)?T(?:(\d+)H)?(?:(\d+)M)?(?:(\d+)(?:[\.,](\d{1,3}))?S)?$/)
    entry['duration'] = ((($1.to_i * 24 + $2.to_i) * 60 + $3.to_i) * 60 + $4.to_i) * 1000 + $5&.ljust(3, '0').to_i
  end
  live = video['liveStreamingDetails']
  if live
    entry['live'] = true
    start_time = live['actualStartTime']
    if start_time
      entry['time'] = Time.iso8601(start_time).to_i
    else
      entry['upcoming'] = true
      entry['time'] = Time.iso8601(live['scheduledStartTime']).to_i
    end
  end
end

feeds.map do |channel, feed|
  open("data/#{channel}.json", 'w') do |out|
    JSON.dump(feed, out)
  end
end
