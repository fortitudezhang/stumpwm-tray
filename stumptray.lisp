(cl:in-package :stumpwm)
;;(require :simpletray)

(defcommand destroy-tray () ()
    "Destroy the system tray."
    (simpletray:destroy))

(defcommand create-tray () ()
    "Create the system tray."
    (simpletray:create))

(defcommand show-tray () ()
    "Shows the system tray."
    (simpletray:show))

(defcommand hide-tray () ()
    "Hides the system tray."
    (simpletray:hide))

(defcommand toggle-tray () ()
    "Toggles tray visibility."
    (simpletray:toggle))

