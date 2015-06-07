(ns neko.notify
  "Provides convenient wrappers for Toast and Notification APIs."
  (:import [android.app Notification NotificationManager PendingIntent]
           [android.content Context Intent]
           android.widget.Toast
           neko.App))

;; ### Toasts

(defn toast
  "Creates a Toast object using a text message and a keyword representing how
  long a toast should be visible (`:short` or `:long`). If length is not
  provided, it defaults to :long."
  ([message]
   (toast App/instance message :long))
  ([message length]
   (toast App/instance message length))
  ([^Context context, ^String message, length]
   {:pre [(or (= length :short) (= length :long))]}
   (.show
    ^Toast (Toast/makeText context message ^int (case length
                                                  :short Toast/LENGTH_SHORT
                                                  :long Toast/LENGTH_LONG)))))

;; ### Notifications

(defn- ^NotificationManager notification-manager
  "Returns the notification manager instance."
  ([^Context context]
   (.getSystemService context Context/NOTIFICATION_SERVICE)))

(defn construct-pending-intent
  "Creates a PendingIntent instance from a vector where the first
  element is a keyword representing the action type, and the second
  element is a action string to create an Intent from."
  ([context [action-type, ^String action]]
     (let [^Intent intent (Intent. action)]
       (case action-type
         :activity (PendingIntent/getActivity context 0 intent 0)
         :broadcast (PendingIntent/getBroadcast context 0 intent 0)
         :service (PendingIntent/getService context 0 intent 0)))))

(defn notification
  "Creates a Notification instance. If icon is not provided uses the
  default notification icon."
  ([options]
   (notification App/instance options))
  ([context {:keys [icon ticker-text when content-title content-text action]
             :or {icon android.R$drawable/ic_dialog_info
                  when (System/currentTimeMillis)}}]
   (let [notification (Notification. icon ticker-text when)]
     (.setLatestEventInfo notification context content-title content-text
                          (construct-pending-intent context action))
     notification)))

(defn fire
  "Sends the notification to the status bar. ID can be an integer or a keyword."
  ([id notification]
   (fire App/instance id notification))
  ([context id notification]
   (let [id (if (keyword? id)
              (Math/abs (.hashCode id))
              id)]
     (.notify (notification-manager context) id notification))))

(defn cancel
  "Removes a notification by the given ID from the status bar."
  ([id]
   (cancel App/instance id))
  ([context id]
   (.cancel (notification-manager context) (if (keyword? id)
                                             (Math/abs (.hashCode id))
                                             id))))
