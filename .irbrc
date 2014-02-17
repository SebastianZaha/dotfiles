# https://github.com/janlelis/irbtools
require 'irbtools'
# This require does not work for some reason
# require 'irbtools/more'

require 'binding.repl'

# ls
#   img1.jpg img2.jpg
# rename_all(/\.jpg/, '.new.jpg')
# ls
#   img1.new.jpg img2.new.jpg
def rename_all(pattern, replace)
  Dir.new('.').each do |f|
    File.rename(f, f.gsub(pattern, replace))  if f.match(pattern)
  end
end

# Resizes all .jpg files in the current dir to fit the size constraint.
def resize_all(size_constraint)
  require 'rmagick'

  Dir.new('.').each do |f|
    if f.match(/jpg/)
      if (i = Magick::Image.read(f).first)
        i.resize_to_fit!(size_constraint)
        i.write(f)
      end
    end
  end
end

# Shortcuts
def repl
  binding.repl.irb
end
