## Install
```
cd cryptoconv-hs/
cabal install
```


## Usage

Run converter with names of two crypto-currencies as parameters and it will calculate current ratio and USD prices.

```
> cryptoconv bitcoin ethereum

by USD price: 1 BTC = 20.73075236257684 ETH
by BTC price: 1 BTC = 20.31252856449329 ETH

Fiat prices:
BTC: 14322.4 USD
ETH: 690.877 USD
>  
```

Names could be found on [coinmarketcap.com](https://coinmarketcap.com).