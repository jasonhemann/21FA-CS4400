#!/usr/bin/env ruby
require 'json'
require 'pathname'
require 'yaml'
require 'liquid'

ROOT = Pathname.new(__dir__).realpath
CONFIG_PATH = ROOT.join('_config.yml')
SYLLABUS_PATH = ROOT.join('syllabus.md')
DEFAULT_OUTPUT_PATH = ROOT.join('syllabus_rendered.md')

def load_structured_file(path)
  case path.extname
  when '.yml', '.yaml'
    YAML.load_file(path, aliases: true)
  when '.json'
    JSON.parse(path.read(encoding: 'UTF-8'))
  else
    nil
  end
end

def load_site_data(root)
  data = {}

  Dir[root.join('_data', '*').to_s].sort.each do |entry|
    path = Pathname.new(entry)
    next unless path.file?

    key = path.basename(path.extname).to_s
    data[key] = load_structured_file(path)
  end

  data
end

def resolve_author(site_config, site_data)
  author_key = site_config['author']
  authors = site_data['authors']

  return site_config unless author_key.is_a?(String)
  return site_config unless authors.is_a?(Hash)
  return site_config unless authors.key?(author_key)

  site_config.merge('author' => authors[author_key])
end

def extract_front_matter(content)
  parts = content.split(/^---\s*$/, 3)
  raise 'syllabus.md is missing YAML front matter' if parts.length < 3

  [parts[1], parts[2]]
end

def rewrite_local_asset_paths(rendered, baseurl)
  normalized_baseurl = baseurl.to_s.sub(%r{/\z}, '')
  asset_prefix = normalized_baseurl.empty? ? '/assets/' : "#{normalized_baseurl}/assets/"

  rendered.gsub(asset_prefix, 'assets/')
end

output_path = if ARGV[0]
                Pathname.new(ARGV[0]).expand_path(Dir.pwd)
              else
                DEFAULT_OUTPUT_PATH
              end

site_config = YAML.load_file(CONFIG_PATH, aliases: true)
site_data = load_site_data(ROOT)
site_config = resolve_author(site_config, site_data)

content = SYLLABUS_PATH.read(encoding: 'UTF-8')
front_matter, body = extract_front_matter(content)
page_data = YAML.safe_load(front_matter, aliases: true) || {}

template = Liquid::Template.parse(body)
context = { 'site' => site_config.merge('data' => site_data), 'page' => page_data }
rendered = template.render(context)
rendered = rewrite_local_asset_paths(rendered, site_config['baseurl'])

File.open(output_path, 'w:UTF-8') do |file|
  file.puts '---'
  file.puts front_matter.strip
  file.puts '---'
  file.puts rendered
end

puts "Rendered Markdown written to #{output_path}"
