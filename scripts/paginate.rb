#!/usr/bin/env ruby
# frozen_string_literal: true

PAGE_SIZE = 50

require 'fileutils'
require 'json'
require 'set'

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

# Detect collab members by `@`-mentions in the description.
entries.each do |entry|
  description = entry.delete('description')
  break unless description
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

entries.sort_by! do |entry|
  -entry['time']
end

# Group multiple streams corresponding to a single collab.
# Here a collab is defined as a set of streams that are scheduled for/started at around a same time,
# referring to each other with `@`-mentions.
entry_groups = []
until entries.empty?
  first = entries.pop

  simul_entries = [first]
  until entries.empty? or entries.last['time'] - first['time'] > 10 * 60
    simul_entries.unshift(entries.pop)
  end

  if simul_entries.length <= 1
    entry_groups.unshift(first)
    next
  end

  # Factorize the streams into weakly connected components in a graph of `@`-mentions:
  graph = simul_entries.map do |entry|
    [entry['feed'], Set.new(entry['members'])]
  end.to_h

  # Convert the graph into an undirected graph.
  graph.each_pair do |vertice, neighbors|
    neighbors.each do |n|
      graph[n].add(vertice)
    end
  end

  # Get the connected components by a breadth-first search.
  vertices = Set.new(graph.keys)
  components = []
  while start = vertices.each.first
    component = Set.new
    queue = [start]
    until queue.empty?
      vertice = queue.shift
      component.add(vertice)
      vertices.delete(vertice)
      graph[vertice].each do |neighbor|
        queue.push(neighbor) if vertices.include?(neighbor)
      end
    end
    components.push(component)
  end

  components.each do |collab_members|
    collab_entries = collab_members.flat_map do |feed|
      simul_entries.select do |entry|
        entry['feed'] == feed
      end
    end

    if collab_members.length > 1
      entry_groups.unshift(collab_entries)
    elsif collab_entries.length > 1
      # A single user uploading multiple videos around a same time,
      # which may not necessarily be relevant to each other.
      entry_groups.unshift(*collab_entries)
    else
      entry_groups.unshift(collab_entries.first)
    end
  end
end

# Like `Enumerable#each_slice(PAGE_SIZE).to_a`, but splits by the size of flattened page instead.
pages = entry_groups.reverse_each.with_object([[]]) do |entry_or_collab, pages|
  prepend_to_existing = if entry_or_collab.is_a?(Array)
    # Allow excess of up to half the length to make the page sizes around `PAGE_SIZE` by average.
    pages.first.flatten.length + entry_or_collab.length / 2 <= PAGE_SIZE
  else
    pages.first.length + 1 <= PAGE_SIZE
  end
  if prepend_to_existing
    pages.first.unshift(entry_or_collab)
  else
    pages.unshift([entry_or_collab])
  end
end

if pages[1] and pages[0].flatten.length < PAGE_SIZE
  # Make sure that the first page contains at least `PAGE_SIZE`.
  latest = pages.shift
  pages[0].unshift(*latest)
end

FileUtils.mkdir_p('feed')
file = 'latest.json'
while page = pages.shift
  STDERR.puts "Writing #{page.flatten.length} entries to `#{file}`..."
  open("feed/#{file}", 'w') do |out|
    json = {}
    if meta
      json['meta'] = meta
      meta = nil
    end
    json['next'] = file = "#{pages.length}.json" unless pages.empty?
    json['entry_groups'] = page
    JSON.dump(json, out)
  end
end
