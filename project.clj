(defproject xscreenbane "0.1.0-SNAPSHOT"
  :description "XScreenBane - more hacks for your XScreenSaver"
  :url ""
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [net.java.dev.jna/jna "5.9.0"]
                 [net.java.dev.jna/jna-platform "5.9.0"]
                 [clojure2d "1.4.5"]
                 [generateme/fastmath "2.2.1" :exclusions [com.github.haifengl/smile-mkl org.bytedeco/openblas]]
                 [com.github.oshi/oshi-core "6.6.3"]
                 [com.rpl/specter "1.1.4"]
                 ]
  :main ^:skip-aot xscreenbane.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
