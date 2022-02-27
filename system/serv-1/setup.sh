#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

# update current system
sudo apt-get -qq update -y
sudo apt-get -qq upgrade -y

# install different utils and components
sudo apt-get -qq install -y tmux

# make ansible happy with python interpreter
if [ ! -f "/usr/bin/python" ];
then
   if [ -f "/usr/bin/python3.9" ];
   then     
     sudo ln -f -s /usr/bin/python3.9 /usr/bin/python
   fi

   if [ -f "/usr/bin/python3.10" ];
   then     
     sudo ln -f -s /usr/bin/python3.10 /usr/bin/python
   fi
fi

# Global values
NETWORKD_INTERFACE_PATH="/etc/systemd/network/wlp0s20f3.network"
WIFI_SUPPLICANT_CONF_PATH="/etc/wpa_supplicant/wpa_supplicant-wlp0s20f3.conf"
WIFI_INTERFACE=$(ip a | grep -B 1 "84:5c:f3:a8:3b:35" | awk '{split($2, str, ":"); print str[1]; exit}')

# Check is WIFI is up
WIFI_UP=$(ip link show dev "$WIFI_INTERFACE" | grep -q "UP")

if [ "$?" -ne 0 ];
then
    if [ -f "$NETWORKD_INTERFACE_PATH" ];
    then
        echo "$NETWORKD_INTERFACE_PATH already exists" 1>&2;
        exit 1
    fi

    if [ -f "$WIFI_SUPPLICANT_CONF_PATH" ];
    then
        echo "$WIFI_SUPPLICANT_CONF_PATH already exists" 1>&2;
        exit 1
    fi

    # Create both configuration files for networkd and wpa_supplicant
    sudo touch "$NETWORKD_INTERFACE_PATH"
    sudo touch "$WIFI_SUPPLICANT_CONF_PATH"
    
    # Write networkd configuration for our WiFi device
    tee -a "$NETWORKD_INTERFACE_PATH" << END
[Match]
Name="$WIFI_INTERFACE"

[Network]
DHCP=ipv4
END

    # Write wpa_supplicant configuration for our WiFi device
    tee -a "$WIFI_SUPPLICANT_CONF_PATH" << END
ctrl_interface=/var/run/wpa_supplicant
ctrl_interface_group=0
update_config=1

network={
  ssid="<SSID_NAME>"
  psk="<PASSWORD>"
  proto=WPA2
}
END

    # Start networkd and wpa_supplicant
    systemctl enable systemd-networkd
    systemctl start systemd-networkd
    systemctl enable wpa_supplicant
    systemctl start wpa_supplicant

    # Satr wpa_supplicant
    systemctl enable wpa_supplicant@"$WIFI_INTERFACE".service
    systemctl start wpa_supplicant@"$WIFI_INTERFACE".service
fi

exit 0
