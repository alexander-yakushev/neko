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
  "Utility for logging in Android. 

    (neko.log/e \"Some log string\" x \"!\" :exception error)

  is equivalent to:

    (android.util.Log/d \"current.namespace\" (pr-str \"Some log string: \" x \"!\") error)

  Where the :exception is entirely optional."
  {:author "Adam Clements"})

(defn- isnt-any-of [& args]
  (let [args (set args)]
    (fn [x] (not (contains? args x)))))

(defn- logger [logfn args]  
  (let [[strings {:keys [exception tag]}] (split-with (isnt-any-of :exception :tag) args)
        my-ns (str *ns*)]
    (if exception
      (list `. `android.util.Log
            (list logfn
                  (or tag my-ns)
                  (concat (list 'clojure.core/pr-str) strings)
                  exception))
      (list `. `android.util.Log
            (list logfn
                  (or tag my-ns)
                  (concat (list 'clojure.core/pr-str) strings))))))

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
