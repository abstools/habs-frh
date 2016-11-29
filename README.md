# Installing prerequisites

On Ubuntu ==16.04:

```bash
# for docker
sudo apt-get update
sudo apt-get install apt-transport-https ca-certificates linux-image-extra-$(uname -r) linux-image-extra-virtual
sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
echo "deb https://apt.dockerproject.org/repo ubuntu-xenial main" | sudo tee /etc/apt/sources.list.d/docker.list
sudo apt-get update
sudo apt-get install docker-engine

# python and haskell
sudo apt-get install python3 python3-pip ghc cabal-install happy zlib1g-dev 
pip3 install setuptools 
pip3 install requests modgrammar
git submodule update --init --recursive
```


```bash
cd habs/

cabal sandbox init
cabal sandbox add-source habs-parser
cabal sandbox add-source habs-stdlib
cabal sandbox add-source habs-runtime

cabal update
cabal install habs-runtime -fwait-all-cogs
cabal install
```

# Compiling the FRH case study

```bash
cd habs/
# (optional) compiling ABS to Haskell
#cabal exec habs -- ../src/*.abs -o ../src/gen/haskell
# manual modifications

cabal exec ghc -- --make -O src/gen/haskell/ABS/*.hs src/gen/haskell/FRH.hs -main-is FRH 
```

# Running the FRH case study

```bash
./src/gen/haskell/FRH --unit-time=1,ms --port=8080 -t &
python3 logreplay/logreplay.py logreplay/fredhopper.biz.log proctime customer=gamma,amazonECU=13 http://localhost:8080/call/queryService/invokeWithSize
```