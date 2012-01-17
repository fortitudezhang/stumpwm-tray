(cl:in-package :stumpwm)
(require :simpletray)

(defcommand destroy-tray () ()
    "Destroy the system tray."
    (simpletray:destroy))

(defcommand create-tray () ()
    "Create the system tray."
    (simpletray:create))
