(ns fintwo.guys
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
    [fintwo.pages :refer :all]))



(defn farmer-form []
  [:form
   ;<input type="hidden" name="submitted" value="yes">
   (hidden-field {:class "form-control"} "group-type" "farmer")
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Group name"} "group-name") ;; NOTE abstract this junk further
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Acres"} "acres")
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Fertility"} "fertility")
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Crop type"} "crop-type")
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Skill/stupidity"} "skill-stupidity")
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Harvest Losses"} "harvest-losses")
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Threshing Losses"} "threshing-losses")
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Raiding Losses"} "raiding-losses")
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Animal Losses"} "animal-losses") ;; Animals and bugs that eat it
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Plague Losses"} "plague-losses") ;; Blight and stuff.
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Farmer number"} "farmer-number")
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Farmer consumption"} "farmer-consumption")
   (submit-button "submit button")
   [:h1 {:class "text-success"}]])

(defn soldier-form []
  [:form
   (hidden-field {:class "form-control"} "group-type" "soldier")
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Group name"} "group-name") ;; NOTE abstract this junk further
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Numbers"} "numbers")
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Average pay"} "average-pay")
   (submit-button "submit button")
   [:h1 {:class "text-success"}]])

(defn miner-form []
  [:form
   (hidden-field {:class "form-control"} "group-type" "miner")
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Group name"} "group-name") ;; NOTE abstract this junk further
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Pick axers"} "pick-axers")
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "Average pay"} "average-pay")
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "cart men"} "cart-men")
   (text-field {:class "form-control" :ng-model "groupname" :placeholder "cart rate"} "cart-rate")
   (submit-button "submit button")
   [:h1 {:class "text-success"}]])