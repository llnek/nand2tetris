;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defproject io.czlab/tecs "1.0.0"

  :license {:url "http://www.eclipse.org/legal/epl-v10.html"
            :name "Eclipse Public License"}

  :description "Solutions to the NAND to Tetris course."
  :url "https://github.com/llnek/tecs"

  :dependencies [[io.czlab/basal "1.0.4"]]

  :plugins [[cider/cider-nrepl "0.14.0"]
            [lein-cprint "1.2.0"]
            [lein-codox "0.10.3"]]

  :profiles {:provided {:dependencies [[org.clojure/clojure
                                        "1.8.0" :scope "provided"]]}
             :uberjar {:aot :all}}

  :global-vars {*warn-on-reflection* true}
  :target-path "out/%s"
  :aot :all

  ;:main  czlab.tecs.asm.core
  ;:main  czlab.tecs.vm.core
  ;:main  czlab.tecs.cmp.lexx
  :main  czlab.tecs.cmp.yacc

  :coordinate! "czlab"
  :omit-source true

  :java-source-paths ["src/main/java" "src/test/java"]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :resource-paths ["src/main/resources"]

  :jvm-opts ["-Dlog4j.configurationFile=file:attic/log4j2.xml"
             "-Dczlabloggerflag=true"]

  :javac-options ["-source" "8"
                  "-Xlint:unchecked" "-Xlint:-options" "-Xlint:deprecation"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

