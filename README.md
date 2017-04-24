# Installing prerequisites

On Ubuntu == 16.04:

```bash
# for docker
sudo apt-get update
sudo apt-get install apt-transport-https ca-certificates linux-image-extra-$(uname -r) linux-image-extra-virtual -y
sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
echo "deb https://apt.dockerproject.org/repo ubuntu-xenial main" | sudo tee /etc/apt/sources.list.d/docker.list
sudo apt-get update
sudo apt-get install docker-engine -y

# for haskell 8.0.2
echo "deb http://ppa.launchpad.net/hvr/ghc/ubuntu xenial main" | sudo tee -a /etc/apt/sources.list
echo "deb http://cz.archive.ubuntu.com/ubuntu xenial main" | sudo tee -a /etc/apt/sources.list
echo "deb http://security.ubuntu.com/ubuntu xenial-security main" | sudo tee -a /etc/apt/sources.list
sudo apt-get update
sudo apt-get install ghc-8.0.2 cabal-install-1.24 happy-1.19.5 zlib1g-dev -y --allow-unauthenticated
echo "PATH=$PATH:/opt/ghc/8.0.2/bin:/opt/cabal/1.24/bin:/opt/happy/1.19.5/bin" | tee -a ~/.bashrc
source ~/.bashrc

# other deps
sudo apt-get install unzip git python3 python3-pip default-jdk ant erlang -y
pip3 install setuptools 
pip3 install requests modgrammar
git submodule update --init
# follow the Configuration instructions on metviz/README.md
```


```bash
cd habs/

cabal sandbox init
cabal sandbox add-source habs-parser
cabal sandbox add-source habs-runtime

cabal update
cabal install habs-runtime -fwait-all-cogs
cabal install
```

# (Re)compiling the FRH case study

```bash
cd habs/
cabal exec habs -- ../src/refined/FRH.abs -o ../gen/haskell # or initial/
cabal exec ghc -- --make -O ../gen/haskell/FRH.hs -main-is FRH 
# absc -erlang ../src/refined/FRHErlang.abs -d ../gen/erl  # if Erlang
cd ..
```

# Running the FRH case study

1. Start the case study model

```bash
gen/haskell/FRH --unit-time=1,ms -p 8080
# gen/erl/run -p 8080 # if Erlang
```

2. Start the monitoring

```bash
sudo docker start grafana
python3 metviz/metviz.py --ccf 10 http://localhost:8080/call/monitor/metricHistory
# note when using the Erlang backend use http://localhost:8080/v1/call/...
```

3. Populate the requests by replaying the logs

```bash
# after (re)placing custom_filters.py and fredhopper.biz.log
python3 logreplay/logreplay.py --extra_params customer=gamma,amazonECU=13 --pass_delay logreplay/fredhopper.biz.log proctime http://localhost:8080/call/queryService/invokeWithDelay
# note when using the Erlang backend use http://localhost:8080/v1/call/...
```
