(start-driver {:browser :firefox
               :prefs {"network.proxy.type" 1
                       "network.proxy.socks" "localhost"
                       "network.proxy.socks_port" 9999}})


(open-url "https://loveread.me")
