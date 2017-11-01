;;  Copyright (c) Cosmin Stejerean. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.

(ns
  ^{:author "Cosmin Stejerean",
    :doc "Support for recursively converting Java beans to Clojure and vice versa."}
  clojure.java.data
  (:use [clojure.tools.logging :only (info)]))
        

(def
 ^{:dynamic true,
   :doc "Specify the behavior of missing setters in to-java in the
 default object case, using one of :ignore, :log, :error"}
 *to-java-object-missing-setter* :ignore)

(defmulti to-java (fn [destination-type value] [destination-type (class value)]))
(defmulti from-java class)

(defn- get-property-descriptors [clazz]
  (.getPropertyDescriptors (java.beans.Introspector/getBeanInfo clazz)))

(defn- list-property? [prop-descriptor]
  (= java.util.List (.getPropertyType prop-descriptor)))

(defn- boxed-boolean-property? [prop-descriptor]
  (= Boolean (.getPropertyType prop-descriptor)))

(defn- type-parameter
  "Get the type parameter for a the generic return type of a method."
  [method]
  (-> method
      (.getGenericReturnType)
      (.getActualTypeArguments)
      (first)))

;; getters

(defn- is-getter [method]
  (and method (= 0 (alength (. method (getParameterTypes))))))

(defn- make-getter-fn [method]
  (fn [instance]
    (from-java (.invoke method instance nil))))

(defn- boxed-boolean-getter
  "Construct the getter name from the property and return the getter from 'class'."
  [clazz property-name]
  (let [getter-name (str "is" (.toUpperCase (subs property-name 0 1)) (subs property-name 1 (count property-name)))]
    (.getDeclaredMethod clazz getter-name nil)))

(defn- add-getter-fn
  "JDK > 1.8 has a documented bug in the reflection API that means getReadMethod returns null for property descriptors
  of type Boolean that correctly begin with 'is'. The only solution is a rather ugly workaround where we construct the
  method name from the property name and use getDeclaredMethod on the owner class."
  [the-map prop-descriptor clazz]
  (let [name   (.getName prop-descriptor)
        method (.getReadMethod prop-descriptor)]
    (if (and (is-getter method) (not (= "class" name)))
      (assoc the-map (keyword name) (make-getter-fn method))
      (if (boxed-boolean-property? prop-descriptor)
        (assoc the-map (keyword name) (make-getter-fn (boxed-boolean-getter clazz name)))
        the-map))))

;; setters

(defn- is-setter [method]
  (and method (= 1 (alength (. method (getParameterTypes))))))

(defn- get-setter-type [method]
  (get (.getParameterTypes method) 0))

(defn- make-setter-fn [method]
    (fn [instance value]
      (.invoke method instance (into-array [(to-java (get-setter-type method) value)]))))

(defn- make-list-setter-fn
  [list-method]
  (fn [instance values]
    (let [array-list (.invoke list-method instance nil)]
      (doseq [value values]
        (.add array-list (to-java (type-parameter list-method) value))))))

(defn- add-setter-fn
  "JAXB doesn't generate setters for lists so we need to modify the default add-setter-fn and then intern it."
  [the-map prop-descriptor]
  (let [name   (.getName prop-descriptor)
        method (.getWriteMethod prop-descriptor)]
    (if (is-setter method)
      (assoc the-map (keyword name) (make-setter-fn method))
      (if (list-property? prop-descriptor)
        (assoc the-map (keyword name) (make-list-setter-fn (.getReadMethod prop-descriptor)))
        the-map))))

(defn- add-array-methods [acls]
  (let [cls (.getComponentType acls)
        to (fn [_ sequence] (into-array cls (map (partial to-java cls)
                                                sequence)))
        from (fn [obj] (map from-java obj))]
    (.addMethod to-java [acls Iterable] to)
    (.addMethod from-java acls from)
    {:to to :from from}))

;; common to-java definitions

(defmethod to-java :default [^Class cls value]
  (if (.isArray cls)
                                        ; no method for this array type yet
    ((:to (add-array-methods cls))
     cls value)
    value))

(defmethod to-java [Enum String] [enum value]
           (.invoke (.getDeclaredMethod enum "valueOf" (into-array [String])) nil (into-array [value])))


(defn- throw-log-or-ignore-missing-setter [key clazz]
  (let [message (str "Missing setter for " key " in " (.getCanonicalName clazz))]
    (cond (= *to-java-object-missing-setter* :error)
          (throw (new NoSuchFieldException message))
          (= *to-java-object-missing-setter* :log)
          (info message))))

(defmethod to-java [Object clojure.lang.APersistentMap] [clazz props]
  "Convert a Clojure map to the specified class using reflection to set the properties"
  (let [instance (.newInstance clazz)
        setter-map (reduce add-setter-fn {} (get-property-descriptors clazz))]
    (doseq [[key value] props]
      (let [setter (get setter-map (keyword key))]
        (if (nil? setter)
          (throw-log-or-ignore-missing-setter key clazz)
          (apply setter [instance value]))))
    instance))

;; feature testing macro, based on suggestion from Chas Emerick:
(defmacro ^{:private true} when-available
  [sym & body]
  (try
    (when (resolve sym)
      (list* 'do body))
    (catch ClassNotFoundException _#)))

(defmacro ^{:private true} when-not-available
  [sym & body]
  (try
    (when-not (resolve sym)
      (list* 'do body))
    (catch ClassNotFoundException _#)))

(when-available
 biginteger
 (defmethod to-java [BigInteger Object] [_ value] (biginteger value)))

(when-not-available
 biginteger
 (defmethod to-java [BigInteger Object] [_ value] (bigint value)))

;; common from-java definitions

(defmethod from-java :default [^Object instance]
  "Convert a Java object to a Clojure map"
  (let [clazz (.getClass instance)]
    (if (.isArray clazz)
      ((:from (add-array-methods clazz))
        instance)
      (let [getter-map (reduce #(add-getter-fn %1 %2 clazz) {} (get-property-descriptors clazz))]
        (into {} (for [[key getter-fn] (seq getter-map)] [key (getter-fn instance)]))))))


(doseq [clazz [String Character Byte Short Integer Long Float Double Boolean BigInteger BigDecimal]]
  (derive clazz ::do-not-convert))

(defmacro ^{:private true} defnumber [box prim prim-getter]
  `(let [conv# (fn [_# number#]
                 (~(symbol (str box) "valueOf")
                  (. number# ~prim-getter)))]
     (.addMethod to-java [~prim Number] conv#)
     (.addMethod to-java [~box Number] conv#)))

(defmacro ^{:private true} defnumbers [& boxes]
  (cons `do
        (for [box boxes
              :let [box-cls (resolve box)
                    prim-cls (.get (.getField box-cls "TYPE")
                                   box-cls)
                    ;; Clojure 1.3: (assert (class? box-cls) (str box ": no class found"))
                    _ (assert (class? box-cls))
                    ;; Clojure 1.3: (assert (class? prim-cls) (str box " has no TYPE field"))
                    _ (assert (class? prim-cls))
                    prim-getter (symbol (str (.getName prim-cls) "Value"))]]
          `(defnumber ~box ~(symbol (str box) "TYPE") ~prim-getter))))

(defnumbers Byte Short Integer Long Float Double)

(defmethod from-java ::do-not-convert [value] value)
(prefer-method from-java ::do-not-convert Object)

(defmethod from-java Iterable [instance] (for [each (seq instance)] (from-java each)))
(prefer-method from-java Iterable Object)

(defmethod from-java java.util.Map [instance] (into {} instance))
(prefer-method from-java java.util.Map Iterable)

(defmethod from-java nil [_] nil)
(defmethod from-java Enum [enum] (str enum))

;; definitions for interfacting with XMLGregorianCalendar

(defmethod to-java [javax.xml.datatype.XMLGregorianCalendar clojure.lang.APersistentMap] [clazz props]
  "Create an XMLGregorianCalendar object given the following keys :year :month :day :hour :minute :second :timezone"
  (let [instance (.newInstance clazz)
        undefined javax.xml.datatype.DatatypeConstants/FIELD_UNDEFINED
        getu #(get %1 %2 undefined)]
    (doto instance
      (.setYear (getu props :year))
      (.setMonth (getu props :month))
      (.setDay (getu props :day))
      (.setHour (getu props :hour))
      (.setMinute (getu props :minute))
      (.setSecond (getu props :second))
      (.setTimezone (getu props :timezone)))))

(defmethod from-java javax.xml.datatype.XMLGregorianCalendar [obj]
  "Turn an XMLGregorianCalendar object into a clojure map of year, month, day, hour, minute, second and timezone "
  (let [date {:year (.getYear obj)
              :month (.getMonth obj)
              :day (.getDay obj)}
        time {:hour (.getHour obj)
              :minute (.getMinute obj)
              :second (.getSecond obj)}
        tz {:timezone (.getTimezone obj)}
        is-undefined? #(= javax.xml.datatype.DatatypeConstants/FIELD_UNDEFINED %1)]
    (conj {}
          (if-not (is-undefined? (:year date))
            date)
          (if-not (is-undefined? (:hour time))
            time)
          (if-not (is-undefined? (:timezone tz))
            tz))))
