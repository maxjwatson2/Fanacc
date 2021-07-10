(ns fintwo.front-end
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
    [clojure.java.io :as io]
    [fintwo.pages :as pag]))

(defn view-details-page [group]
  (let [group-deets (case (be/get-group-type (:guy (:params group)))
                     "farmer" (be/get-farm-details (:guy (:params group)))
                     "soldier" (be/get-soldier-details (:guy (:params group))))]
  (html
    "hey look a details page"
    (str group-deets))))

(defroutes app-routes
           (GET "/" [] pag/front-page)
           (GET "/addguy/:group" [group] (pag/add-group-page group))
           (GET "/guydeets/:guy" req (pag/view-guy-page req))
           (GET "/realdeets/:guy" req (view-details-page req))
           (route/resources "/")
           #_(ANY "*" [] front-page))

(defn start-server []
  (run-jetty app-routes {:port 3000 :join? false}))