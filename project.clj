(defproject adb/java.data "0.1.2-SNAPSHOT"
  :description "A fork of java.data with fixes for JAXB source generation issues."
  :license {:name "Eclipse Public License"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.logging "0.4.0"]]
  :plugins [[s3-wagon-private "1.3.4"]]
  :deploy-repositories [["releases" {:url "s3p://elcom-maven-repo/release/" :no-auth true :sign-releases false}]]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure" "src/test/java"]
  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit" "Version %s"]
                  ["vcs" "tag" "--no-sign"]
                  ["deploy"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit" "Bumping version to %s"]
                  ["vcs" "push"]])
