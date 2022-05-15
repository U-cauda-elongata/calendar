#!/usr/bin/env ruby
# frozen_string_literal: true

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
      'entries' => @entries.values,
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
    res.body
  end
end.map do |xml|
  Feed.parse(xml)
end

ids = feeds.flat_map {|feed| feed.entries.keys }
uri = URI('https://youtube.googleapis.com/youtube/v3/videos')
http = Net::HTTP.new(uri.host, uri.port)
http.use_ssl = uri.scheme == 'https'
items = http.start do
  ids.each_slice(50).with_object([]) do |slice, acc|
    uri.query = URI.encode_www_form([
      ['part', 'liveStreamingDetails'],
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

entries = feeds.each_with_object({}) do |feed, acc|
  acc.merge!(feed.entries)
end

items.each do |video|
  live = video['liveStreamingDetails']
  if live
    entry = entries[video['id']]
    actual = live['actualStartTime']
    if actual
      entry[:time] = Time.iso8601(actual).to_i
    else
      entry[:upcoming] = true
      entry[:time] = Time.iso8601(live['scheduledStartTime']).to_i
    end
  end
end

JSON.dump(channels.zip(feeds).to_h, STDOUT)
