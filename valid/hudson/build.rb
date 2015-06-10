#!/usr/bin/ruby

$LOAD_PATH.push('metabuild/lib')
require 'metabuild'
include Metabuild

options = Options.new({ "target"     => "k1",
                        "version"    => ["undef", "Version of the delivered tools."],
                        "output-dir" => ["", "Where produced RPMs are stored"],
                        "prefix"     => ["devimage", "Path to install latex."],
                      })

workspace  = options["workspace"]
lizard_clone = options['clone']
lizard_path  = File.join(workspace,lizard_clone)

output_dir = options["output-dir"]

repo = Git.new(lizard_clone,workspace)

prefix          = options["prefix"] == "devimage" ? File.expand_path(options["prefix"],workspace) : options["prefix"]
default_prefix = File.expand_path(options["prefix"],workspace)
prefix = options["prefix"] == "devimage" ? default_prefix : options["prefix"]
prefix = File.join(prefix,"lizard")

clean = CleanTarget.new("clean", repo, [])
build = ParallelTarget.new("build", repo, [], [])
valid = ParallelTarget.new("valid", repo, [build], [])
install = Target.new("install", repo, [valid], [])
install.write_prefix()

package = Target.new("package", repo, [], [])
package.write_prefix()

b = Builder.new("lizard", options, [clean, build, valid, install, package])

flag_options  = []
lib_options   = []

b.default_targets = [install]

arch = options["target"]
b.logsession = arch

version = "1.0"

b.target("build") do
        b.logtitle = "Report for lizard build"
end

b.target("clean") do
        b.logtitle = "Report for lizard clean"
end

b.target("valid") do
        b.logtitle = "Report for lizard valid"

        #~ lizard_build_dirs.each do |build_dir|
                #~ cd build_dir
        #~ end
end

b.target("install") do
        b.logtitle = "Report for lizard install"
        cd lizard_path
        b.run(:cmd => "python setup.py install --root=#{prefix}", :msg => "Error while installing lizard")
end

b.target("package") do
  b.logtitle = "Report for lizard packaging"

  mkdir_p output_dir unless File.exists?(output_dir)
  
  b.cd! prefix
  
  b.run("tar cf lizard.tar ./*")
  tar_package = File.expand_path("lizard.tar")
  
  package_description = "Lizard package\n"
  package_description += "This package provides lizard code analyzer."
  
  depends = []
  
  release_info = b.release_info(version, "0")

  pinfo = b.package_info("k1-lizard", release_info, package_description,
                         "/usr/local/k1tools/kalray_internal", output_dir, depends)
  
  b.create_package(tar_package, pinfo)
  b.run("rm -f #{tar_package}")
end

b.launch


