(ns user
  (:require [malli.dev]))

(alter-var-root #'*warn-on-reflection* (constantly true))
(malli.dev/start!)

(comment
  (require '[malli.clj-kondo])
  (malli.clj-kondo/emit!))
