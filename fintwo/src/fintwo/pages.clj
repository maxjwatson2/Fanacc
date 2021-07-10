(ns fintwo.pages
  (require
    [compojure.core :refer :all]
    [compojure.route :as route]
    [cheshire.core :refer :all]
    [ring.adapter.jetty :refer [run-jetty]]
    [hiccup.core :refer :all]
    [hiccup.page :refer :all]
    [hiccup.form :refer :all]
    [reagent.core :refer :all]
    [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
    [clojure.string :as str]
    [fintwo.back-end :as be]
    [fintwo.helpers :refer :all]
    [clojure.java.io :as io]
    [fintwo.pages :refer :all]
    [fintwo.guys :refer :all]))

(def group-count (atom 0))

(def groups (atom []))

(defn add-group-backend [group] ;; NOTE this should also be on the back end
  (swap! groups conj [:div [:a {:href (str "/guydeets/" (str group @group-count))} (str group @group-count)]]))

(defn add-group [group] ;; NOTE this should be back end eventually
  (swap! group-count inc)
  (case group ;; NOTE Abstract this more. We have a lot of the devil's clip board.
    "miner" (add-group-backend group)
    "farmer"(add-group-backend group)
    "smith" (add-group-backend group)
    "soldier" (add-group-backend group)))

(defn all-saved-groups []
  (for [fil (drop 1 (file-seq (clojure.java.io/file (io/resource "saved"))))]
    [:div [:a {:href (str "/realdeets/" (str (:group-name (process-string (slurp fil)))))} (str (:group-name (process-string (slurp fil))))]]))

(defn get-total-yield []
  (remove nil?
          (for [fil (drop 1 (file-seq (clojure.java.io/file (io/resource "saved"))))]
            (if (= "farmer" (:group-type (process-string (slurp fil))))
              (be/get-farm-details (:group-name (process-string (slurp fil))))))))

(defn get-total-salary []
  (remove nil?
          (for [fil (drop 1 (file-seq (clojure.java.io/file (io/resource "saved"))))]
            (if (= "soldier" (:group-type (process-string (slurp fil))))
              (be/get-soldier-details (:group-name (process-string (slurp fil))))))))

(def group-buttons
  [[:div [:a {:href "/addguy/miner"} "Add Miner"]]
   [:div [:a {:href "/guydeets/farmer"} "Add Farmer"]]
   [:div [:a {:href "/addguy/smith"} "Add Smith"]]
   [:div [:a {:href "/guydeets/soldier"} "Add Soldier"]]])

(defn add-group-page [group]
  (add-group group)
  (html
    (for [g group-buttons] g)
    (for [g @groups] g)))

(defn front-page [req]
  (html
    (for [g group-buttons] g)
    [:br]
    [:div
     "Total yield: "
     [:strong (reduce + (for [y (get-total-yield)]
                          (:net-yield y)))]]
    [:br]
    [:div
     "Total potential yield: "
     [:strong (reduce + (for [y (get-total-yield)]
                          (:potential-yield y)))]]
    [:br]
    [:div "Total GDP in brouzouf: "
     [:strong (/ (reduce + (for [y (get-total-yield)]
                             (:net-yield y))) 30)]]
    [:br]
    [:div "Ten Pecent tax on GDP in brouzouf: "
     [:strong (/ (/ (reduce + (for [y (get-total-yield)]
                                (:net-yield y))) 30) 10)]]
    [:br]
    [:div "Total Daily Salary in Brouzouf: "
     [:strong (reduce + (for [y (get-total-salary)]
                          (:total-pay-daily y)))]]
    [:br]
    [:div "Total Seasonal Salary in Brouzouf: "
     [:strong (reduce + (for [y (get-total-salary)]
                          (:total-pay-seasonal y)))]]))

(defn view-guy-page [guy]
  (html
    (:guy (:params guy))
    (if (> (count (:query-string guy)) 25)                  ;; NOTE this is kind of a hack but whatever.
      (str (be/new-farmer-to-backend (:query-string guy) (:group-name (process-string (:query-string guy))))
           "Guy added!"))

    (case (:guy (:params guy))
      "farmer" (farmer-form)
      "soldier" (soldier-form))

    (all-saved-groups)
    #_(for [fil (drop 1 (file-seq (clojure.java.io/file (io/resource "saved"))))]
      [:div [:a {:href (str "/realdeets/" (str (:group-name (process-string (slurp fil)))))} (str (:group-name (process-string (slurp fil))))]])))