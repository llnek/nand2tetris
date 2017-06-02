;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.test.loki.test

  (:require [czlab.loki.game.core :as gc]
            [czlab.loki.game.room :as gr]
            [czlab.loki.game.reqs :as rs]
            [clojure.java.io :as io]
            [clojure.string :as cs]
            [czlab.loki.player :as p]
            [czlab.loki.util :as u]
            [czlab.basal.meta :as m]
            [czlab.wabbit.xpis :as xp]
            [czlab.basal.format :as f]
            [czlab.basal.core :as c]
            [czlab.basal.str :as s]
            [czlab.loki.xpis :as loki]
            [czlab.loki.session :as ss]
            [czlab.loki.net.core :as nc])

  (:use [clojure.test])

  (:import [czlab.jasal
            Initable
            Startable
            Disposable
            LifeCycle
            Idable
            Restartable
            Hierarchical]
           [czlab.basal Cljrt]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  evt-json
  "{\"type\" : 2, \"status\": 200, \"code\":911, \"body\": { \"a\" : 911 }}")
(def ^:private evt-body {:a 911})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mockSocket "" []
  (m/new<> "io.netty.channel.embedded.EmbeddedChannel"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mockDelegate "" []
  (reify
    loki/GameImpl
    (get-player-gist[_ _])
    (on-game-event [_ _])
    (start-round [_ _])
    (end-round [_])
    Restartable
    (restart [_ _])
    (restart [_])
    Initable
    (init [_ _])
    Startable
    (start [_ _])
    (start [_])
    (stop [_])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mockExec "" []
  (let [rts (Cljrt/newrt)]
    (reify
      xp/Execvisor
      (has-child? [_ id] )
      (get-child [_ id] )
      (uptime-in-millis [_] )
      (get-locale [_] )
      (get-start-time [_] )
      (kill9! [_] )
      (cljrt [_] rts)
      (get-scheduler [_] )
      (get-home-dir [_] )

      xp/KeyAccess
      (pkey-bytes [this] (c/bytesit "hello world"))
      (pkey-chars [_] (.toCharArray "hello world"))

      Idable
      (id [_] "1")

      Startable
      (start [this _] )
      (start [this] )
      (stop [this] )

      xp/SqlAccess
      (acquire-db-pool [_ gid] nil)
      (acquire-db-api [_ gid] nil)
      (dft-db-pool [_] nil)
      (dft-db-api [_] nil)

      Disposable
      (dispose [this]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- mockPluglet "" []
  (let [exe (mockExec)]
    (reify
      Hierarchical
      (parent [_] exe)
      Idable
      (id [_] "?")
      LifeCycle
      (init [_ _] )
      (start [_] )
      (start [_ _])
      (dispose [_] )
      (stop [_]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  games-meta
  {:game-1
   {:layout "portrait",
    :name  "Test",
    :description "Fun!",
    :keywords "",
    :height  480,
    :width  320
    :pubdate #inst "2016-01-01"
    :author "joe"
    :network {
      :enabled? true
      :minp 2
      :maxp 2
      :impl  :czlab.test.loki.test/testArena}
    :image "ui/catalog.png"}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn testArena "" [_ _ ] (mockDelegate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestloki-test

  (require 'czlab.nettio.core)
  (is (c/do->true
        (gc/initGameRegistry! games-meta)))

  (is (let [evt (nc/eventObj<> ::loki/public ::loki/quit evt-body {:x 7})]
        (and (= 2 (get loki/loki-msg-types (:type evt)))
             (= ::loki/ok (:status evt))
             (= 911  (get loki/loki-msg-codes (:code evt)))
             (map? (:body evt))
             (== 7 (:x evt)))))

  (is (let [evt (nc/errorObj<> ::loki/public ::loki/quit evt-body {:x 7})]
        (and (= 2 (get loki/loki-msg-types (:type evt)))
             (= ::loki/error (:status evt))
             (= 911 (get loki/loki-msg-codes (:code evt)))
             (map? (:body evt))
             (== 7 (:x evt)))))

  (is (let [evt (-> (nc/eventObj<> ::loki/public ::loki/quit evt-body)
                    nc/encodeEvent)
            s? (string? evt)]
        (and s?
             (> (.indexOf evt "2") 0)
             (> (.indexOf evt "200") 0)
             (> (.indexOf evt "911") 0))))

  (is (let [evt (-> (nc/eventObj<> ::loki/public ::loki/quit)
                    nc/encodeEvent)
            s? (string? evt)]
        (and s?
             (> (.indexOf evt "2") 0)
             (> (.indexOf evt "200") 0)
             (> (.indexOf evt "911") 0))))

  (is (let [evt (nc/decodeEvent evt-json {:x 3})]
        (and (= ::loki/public (:type evt))
             (= ::loki/quit (:code evt))
             (== 911 (get-in evt [:body :a])))))

  (is (some? (gc/lookupGame "game-1")))

  (is (let [c1 (p/lookupPlayer "u1" "p1") ;; user#1
            c2 (p/lookupPlayer "u1")
            c3 (p/removePlayer "u1")
            c4 (p/lookupPlayer "u1")]
        (and (some? c1)
             (identical? c1 c2)
             (some? c3)
             (nil? c4))))

  (is (let [c1 (p/lookupPlayer "u1" "p1") ;; user#2
            c1 (assoc c1 :email "e" :name "n")
            e (:email c1)
            n (:name c1)
            nn (:userid c1)
            id (c/id?? c1)
            cs (ss/countSessions c1)
            _ (p/logout c1)
            c4 (p/lookupPlayer "u1")]
        (and (some? c1)
             (= e "e")
             (= n "n")
             (== 0 cs)
             (= nn "u1")
             (not= nn id)
             (nil? c4))))

  (is (let [gid "game-1"
            s (rs/doPlayReq {:source (mockPluglet)
                             :socket (mockSocket)
                             :body {:gameid gid
                                    :settings {:a 1}
                                    :principal  "u1"
                                    :credential "p1"}}) ;user#3
            t (rs/doPlayReq {:source (mockPluglet)
                             :socket (mockSocket)
                             :body {:gameid gid
                                    :settings {:b 2}
                                    :principal  "u2" ;user#4
                                    :credential "p2"}})
            gid (keyword gid)
            r1 (:roomid (some-> s deref))
            r2 (:roomid (some-> t deref))
            ok
            (and (some? r1)
                 (some? r2)
                 (= r1 r2)
                 (= 1 (:a @s))
                 (= 2 (:b @t))
                 (== 1 (gr/countGameRooms gid))
                 (== 0 (gr/countFreeRooms gid))
                 (not (loki/can-open-room? (gr/lookupGameRoom gid r1))))
            _ (gr/clearFreeRooms gid)
            _ (gr/clearGameRooms gid)]
        (and ok
             (== 0 (gr/countGameRooms gid))
             (== 0 (gr/countFreeRooms gid)))))

  (is (let [gid "game-1"
            s (rs/doPlayReq {:source (mockPluglet)
                             :socket (mockSocket)
                             :body {:gameid gid
                                    :principal  "u3"
                                    :credential "p3"}}) ;user#5
            gid (keyword gid)
            r (gr/lookupFreeRoom gid
                                 (:roomid (some-> s deref)))
            ok
            (and (some? r)
                 (== 1 (gr/countFreeRooms gid))
                 (not (loki/can-open-room? r)))
            _ (gr/removeFreeRoom gid (c/id?? r))]
        (and ok
             (== 0 (gr/countFreeRooms gid)))))

  (is (let [gid "game-1"
            s (rs/doPlayReq {:source (mockPluglet)
                             :socket (mockSocket)
                             :body {:gameid gid
                                    :principal  "u4"
                                    :credential "p4"}})
            pu4_ok (p/lookupPlayer "u4")
            pu4 (:player @s)
            cnt (ss/countSessions pu4)
            r (gr/lookupFreeRoom (keyword gid)
                                 (:roomid (some-> s deref)))
            na (not (loki/can-open-room? r))
            t (rs/doJoinReq {:source (mockPluglet)
                             :socket (mockSocket)
                             :body {:roomid (s/sname (some-> r c/id??))
                                    :gameid gid
                                    :principal  "u5"
                                    :credential "p5"}})
            gid (keyword gid)
            r2 (gr/lookupGameRoom gid
                                  (:roomid (some-> t deref)))
            _ (p/logout pu4)
            cnt2 (ss/countSessions pu4)
            pu4_nok (p/lookupPlayer "u4")]
        (and (some? r)
             (some? r2)
             (= 1 cnt)
             (= 0 cnt2)
             (some? pu4_ok)
             (nil? pu4_nok)
             na
             (identical? r r2)
             (not (loki/can-open-room? r2))
             (== 1 (gr/countGameRooms gid))
             (== 0 (gr/countFreeRooms gid))
             (c/do->true (gr/clearGameRooms gid))
             (c/do->true (gr/clearFreeRooms gid)))))

  (is (== 90 (u/rad->deg (u/deg->rad 90))))

  (is (string? "That's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

