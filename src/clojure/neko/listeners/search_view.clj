(ns neko.listeners.search-view
  (:require [neko.debug :refer [safe-for-ui]])
  (:use [neko.-utils :only [call-if-nnil]])
  (:import android.widget.SearchView))

(defn on-query-text-call
  "Takes onQueryTextChange and onQueryTextSubmit functions and yields
  a SearchView.OnQueryTextListener object that will invoke the
  functions. Both functions take string argument, a query that was
  entered."
  ^android.widget.SearchView$OnQueryTextListener
  [change-fn submit-fn]
  (reify android.widget.SearchView$OnQueryTextListener
    (onQueryTextChange [this query]
      (safe-for-ui (call-if-nnil change-fn query)))
    (onQueryTextSubmit [this query]
      (safe-for-ui (call-if-nnil submit-fn query)))))
