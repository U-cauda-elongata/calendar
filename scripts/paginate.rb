#!/usr/bin/env ruby
# frozen_string_literal: true

PAGE_SIZE = 50

require 'fileutils'
require 'json'
require 'set'

handle_to_id = {
  'けものフレンズプロジェクト公式' => 'yt:channel:UCEOugXOAfa-HRmRjKbH8z3Q',
  'parkstaff9305' => 'yt:channel:UCrMKWvQuyFL1dckG2RLInDQ',
  'humboldtpenguin2619' => 'yt:channel:UCmYO-WfY7Tasry4D1YB4LJw',
  'islandfox6864' => 'yt:channel:UCMpw36mXEu3SLsqdrJxUKNA',
  'Coyote_KemoV' => 'yt:channel:UCabMjG8p6G5xLkPJgEoTnDg',
  'direwolf8958' => 'yt:channel:UCdNBhcAohYjXlUVYsz8X2KQ',
  'caracal4893' => 'yt:channel:UCxm7yNjJsSvyvcG96-Cvmpw',
  'large-spottedgenet4617' => 'yt:channel:UCNObi6xvj6QeZ0g7BhAbF7w',
  'geoffroyscat4196' => 'yt:channel:UCYa58DdXGAGMJQHqTxi-isA',
  'brownlong-earedbat7015' => 'yt:channel:UCnyE-wD1pE2GZOxA6OHjW9g',
  'junglecat3723' => 'yt:channel:UCtJSUW-5FnwfaivXpluABWA',
  'siberianchipmunk5236' => 'yt:channel:UCKob71cjOlyYF5bgvtGuNKQ',
  'africanpenguin6535' => 'yt:channel:UCEcMIuGR8WO2TwL9XIpjKtw',
}

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

meta.each do |m|
  handle_to_id[m['title']] ||= m['id']
end

# Detect collab members by `@`-mentions in the description.
entries.each do |entry|
  description = entry.delete('description')
  break unless description
  members = description.each_line.filter_map do |line|
    match = line.match(/.*[@＠](.+?)\s*$/)
    if match
      id = handle_to_id[match[1]]
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
  simul_feeds = Set.new([first['feed']])
  until entries.empty? or entries.last['time'] - first['time'] > 10 * 60
    entry = entries.pop
    simul_entries.unshift(entry)
    simul_feeds.add(entry['feed'])
  end

  if simul_entries.length <= 1
    entry_groups.unshift(first)
    next
  end

  # Factorize the streams into weakly connected components in a graph of `@`-mentions:
  graph = simul_entries.map do |entry|
    [entry['feed'], simul_feeds.intersection(entry['members'] || [])]
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
  open("docs/feed/#{file}", 'w') do |out|
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
