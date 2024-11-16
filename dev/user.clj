(ns user
  (:require [malli.dev]))

(alter-var-root #'*warn-on-reflection* (constantly true))
(malli.dev/start!)
