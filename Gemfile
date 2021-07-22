source "https://rubygems.org"

# Hello! This is where you manage which Jekyll version is used to run.
# When you want to use a different version, change it below, save the
# file and run `bundle install`. Run Jekyll with `bundle exec`, like so:
#
#     bundle exec jekyll serve
#
# This will help ensure the proper Jekyll version is running.
# Happy Jekylling!
# gem "jekyll", "~> 3.8.7"

# This is the default theme for new Jekyll sites. You may change this to anything you like.
gem "minima", "~> 2.0"

# If you want to use GitHub Pages, remove the "gem "jekyll"" above and
# uncomment the line below. To upgrade, run `bundle update github-pages`.
gem "github-pages", "~> 206", group: :jekyll_plugins

# If you have any plugins, put them here!
group :jekyll_plugins do
  gem "jekyll-feed", "~> 0.6"
#  gem 'jekyll-org', '>= 1.1.0'
end

# Windows does not include zoneinfo files, so bundle the tzinfo-data gem
# and associated library.
install_if -> { RUBY_PLATFORM =~ %r!mingw|mswin|java! } do
  gem "tzinfo", "~> 1.2"
  gem "tzinfo-data"
end

# Performance-booster for watching directories on Windows
gem "wdm", "~> 0.1.0", :install_if => Gem.win_platform?


gem "jekyll-include-cache", "~> 0.2.0"

gem "midnight", "~> 1.0"

gem "re-org", "~> 0.0.4"

gem "pygments.rb", "~> 1.2"

# Added b/c CDN mentioned these might be unbundled in 3.0, and need manual adding.
# https://ask.csdn.net/questions/1978947
# https://bugs.ruby-lang.org/issues/16485#change-83794
gem 'rexml'
gem 'rss'
# Added for similar reasons, to appease jekyll
gem 'webrick'


# Added because HTMLPipeline suggested these as dependencies for various Filters
# https://github.com/jch/html-pipeline#dependencies
gem 'rinku'
gem 'escape_utils'
gem 'email_reply_parser'
gem 'gemoji'
gem 'commonmarker'
gem 'sanitize'
gem 'rouge'
gem 'RedCloth'
