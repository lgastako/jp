(ns jp.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [jp.core-test]))

(doo-tests 'jp.core-test)
