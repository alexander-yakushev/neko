; Copyright © 2011 Sattvik Software & Technology Resources, Ltd. Co.
; All rights reserved.
;
; This program and the accompanying materials are made available under the
; terms of the Eclipse Public License v1.0 which accompanies this distribution,
; and is available at <http://www.eclipse.org/legal/epl-v10.html>.
;
; By using this software in any fashion, you are agreeing to be bound by the
; terms of this license.  You must not remove this notice, or any other, from
; this software.

(ns neko.log
  "Utility for logging in Android. There are five logging macros: `i`,
  `d`, `e`, `v`, `w`; for different purposes. Each of them takes
  variable number of arguments and optional keyword arguments at the
  end: `:exception` and `:tag`. If `:tag` is not provided, current
  namespace is used instead. Examples:

    (require '[neko.log :as log])

    (neko.log/d \"Some log string\" {:foo 1, :bar 2})
    (neko.log/i \"Logging to custom tag\" [1 2 3] :tag \"custom\")
    (neko.log/e \"Something went wrong\" [1 2 3] :exception ex)"
  {:author "Adam Clements"}
  (:import android.util.Log))

(defn- logger [logfn args]
  (let [[strings kwargs] (split-with (complement #{:exception :tag}) args)
        {:keys [exception tag]} (if (odd? (count kwargs))
                                  (butlast kwargs)
                                  kwargs)
        my-ns (str *ns*)]
    (if exception
      `(. Log ~logfn ~(or tag my-ns) (pr-str ~@strings) ~exception)
      `(. Log ~logfn ~(or tag my-ns) (pr-str ~@strings)))))

(defmacro e
  "Log an ERROR message, applying pr-str to all the arguments and taking
   an optional keyword :exception or :tag at the end which will print the
   exception stacktrace or override the TAG respectively"
  [& args] (logger 'e args))

(defmacro d
  "Log a DEBUG message, applying pr-str to all the arguments and taking
   an optional keyword :exception or :tag at the end which will print the
   exception stacktrace or override the TAG respectively"
  [& args] (logger 'd args))

(defmacro i
  "Log an INFO message, applying pr-str to all the arguments and taking
   an optional keyword :exception or :tag at the end which will print the
   exception stacktrace or override the TAG respectively"
  [& args] (logger 'i args))

(defmacro v
  "Log a VERBOSE message, applying pr-str to all the arguments and taking
   an optional keyword :exception or :tag at the end which will print the
   exception stacktrace or override the TAG respectively"
  [& args] (logger 'v args))

(defmacro w
  "Log a WARN message, applying pr-str to all the arguments and taking
   an optional keyword :exception or :tag at the end which will print the
   exception stacktrace or override the TAG respectively"
  [& args] (logger 'w args))
