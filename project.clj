(defproject xscreenbane "0.1.0"
  :description "XScreenBane - the bane of boredom for your screen"
  :url ""
  :license {:name "MIT"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [net.java.dev.jna/jna "5.9.0"]
                 [net.java.dev.jna/jna-platform "5.9.0"]
                 ;[generateme/fastmath "2.2.1" :exclusions [com.github.haifengl/smile-mkl org.bytedeco/openblas]]
                 [clojure2d "1.5.0-SNAPSHOT"]
                 [com.github.oshi/oshi-core "6.6.3"]
                 [com.rpl/specter "1.1.4"]
                 [spootnik/signal "0.2.5"]
                 [amalloy/ring-buffer "1.3.1"]
                 ]
  ;:main ^:skip-aot xscreenbane.core
  ;:main xscreenbane.core
  :main xscreenbane.main
  :target-path "target/%s"
  :resource-paths ["resources"]
  :aot [clojure2d.core clojure2d.color]
  :profiles {:uberjar { :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
