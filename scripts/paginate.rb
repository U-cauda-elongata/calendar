#!/usr/bin/env ruby
# frozen_string_literal: true

PAGE_SIZE = 50

require 'fileutils'
require 'json'

(meta, entries) = Dir.glob('data/*.json').each_with_object([[], []]) do |file, (meta, entries)|
  feed = open(file) do |f|
    JSON.load(f)
  end
  file.delete_prefix!('data/')
  file.delete_suffix!('.json')
  channel = file
  feed_id = "yt:channel:#{channel}"
  meta.push({
    'id' => feed_id,
    'title' => feed['title'],
    'alternate' => feed['alternate'],
  })
  feed['entries'].each do |e|
    entry = {}
    entry['id'] = e['id']
    entry['feed'] = feed_id
    entry['name'] = e['name']
    entry['live'] = e['live'] if e['live']
    entry['upcoming'] = e['upcoming'] if e['upcoming']
    entry['time'] = e['time']
    entry['duration'] = e['duration'] if e['duration']
    entry['link'] = e['link'] if e['link']
    entry['thumbnail'] = e['thumbnail'] if e['thumbnail']
    entry['description'] = e['description'] if e['description']
    entries.push(entry)
  end
end

title_to_id = meta.map do |m|
  [m['title'], m['id']]
end.to_h

entries.each do |entry|
  description = entry.delete('description')
  members = description.each_line.filter_map do |line|
    match = line.match(/.*[@＠](.+?)\s*$/)
    if match
      id = title_to_id[match[1]]
      id unless id == entry['feed']
    end
  end
  unless members.empty?
    entry['members'] = members
  end
end

entries.sort_by! do |e|
  e['time']
end

pages = entries.each_slice(PAGE_SIZE).to_a
if pages[-2] and pages[-1].length < PAGE_SIZE
  latest = pages.pop
  pages[-1].concat(latest)
end
pages.each(&:reverse!)
pages.reverse!

FileUtils.mkdir_p('feed')
page = pages.shift || {}
file = 'latest.json'
while page
  STDERR.puts "Writing #{page.length} entries to `#{file}`..."
  open("feed/#{file}", 'w') do |out|
    json = {}
    if meta
      json['meta'] = meta
      meta = nil
    end
    json['next'] = file = "#{pages.length}.json" unless pages.empty?
    json['entries'] = page
    JSON.dump(json, out)
    page = pages.shift
  end
end
