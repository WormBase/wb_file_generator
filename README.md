# wb_file_generator
This file will be mainly used to generate files for WormMine

## install deps
```bash
apt install maven clojure
```

Installs Cojure deps tools: https://www.clojure.org/guides/getting_started

Download datomic-pro
```bash
./bin/maven-install
```



# Set Env Variable

```bash
export WB_DB_URI=datomic:ddb://us-east-1/WS277.1/wormbase
```

Export AWS keys env variables

# Install Dataomic Pro in m2 folder

It is currently installed on the staging server. Unfortunately the staging server does not have enought memory to create the variation file. (/usr/local/wormbase/datomic-pro/datomic-pro-0.9.5703)

# Selecting Files

at the bottom of the intermine.clj file you might want to comment out certain files if the machine does not have enough memory. In the future we could create a way of selecting certain files if that would be useful

## Run Comand

```bash
clj -A:run intermine
```
