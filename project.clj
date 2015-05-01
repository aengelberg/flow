(defproject flow "0.1.2"
  :description "An unofficial solver for the iOS puzzle game \"Flow.\""
  :url "http://github.com/aengelberg/flow"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [seesaw "1.4.3"]
                                        ;[incanter "1.4.1"]
                 [loco "0.3.0-SNAPSHOT"]
                 ]
  ;:java-source-paths ["src/flow/java"]
  :main flow.seesaw)
