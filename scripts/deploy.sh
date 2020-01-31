#!/bin/bash
USER=$(whoami)

echo "removing deployed sc-demo..."
sudo rm -rf /srv/shiny-server/sc-demo
sudo rm -rf /srv/shiny-server/resource
sudo rm -rf /srv/shiny-server/data
sudo mkdir /srv/shiny-server/sc-demo

cd /srv/shiny-server/sc-demo

echo "deploying new sc-demo..."
sudo cp /home/${USER}/sc-demo/src/* ./
sudo cp -r /home/${USER}/sc-demo/data ../
sudo cp -r /home/${USER}/sc-demo/resource ../

echo "restarting shiny-server..."
sudo service shiny-server restart

echo "Deployment is completed!"
