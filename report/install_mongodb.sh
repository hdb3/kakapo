sudo apt-get install gnupg curl
curl -fsSL https://www.mongodb.org/static/pgp/server-8.0.asc | sudo gpg -o /usr/share/keyrings/mongodb-server-8.0.gpg --dearmor
echo "deb [ arch=amd64,arm64 signed-by=/usr/share/keyrings/mongodb-server-8.0.gpg ] https://repo.mongodb.org/apt/ubuntu noble/mongodb-org/8.0 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-8.0.list
sudo apt-get update
sudo apt-get install -y mongodb-org mongodb-mongosh
sudo systemctl enable mongod.service --now
sed -e '1 i \ [' -e '$ s/,$/\]/' < ../kakapo.json > tmp.json
mongoimport --db db_x --collection coll_x --file tmp.json --jsonArray

source activate
pip install matplotlib pymongo
./report.py mongo a

