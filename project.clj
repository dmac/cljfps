(defproject fps "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [org.lwjgl/lwjgl "3.2.0"]
                 [org.lwjgl.lwjgl/lwjgl_util "2.9.3"]
                 [org.lwjgl.lwjgl/lwjgl-platform "2.9.3" :classifier "natives-osx" :native-prefix ""]
                 [slick-util "1.0.0"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]]
                   :plugins [[lein-midje "3.0.1"]]}}
  :jvm-opts ["-Djava.library.path=target/native"]
  :main fps.core)
