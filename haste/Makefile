.PHONY: all single libraries vault

HASTEC=hastec -odir obj -hidir obj

# compile in a single run to avoid dependencies messing up
single : CurrencyConverter/CurrencyConverter.hs
	cd obj && $(HASTEC) \
		-hide-all-packages \
		-package base -package containers -package haste-lib \
		-i../libraries/transformers-0.3.0.0 \
		-i../libraries/vault/src/ \
		-i../libraries/base/ \
		-i../../reactive-banana/src/ \
		../CurrencyConverter/CurrencyConverter.hs
	
# compile all examples
all : CurrencyConverter/CurrencyConverter.js

CurrencyConverter/CurrencyConverter.js : CurrencyConverter/CurrencyConverter.hs
	cd CurrencyConverter && $(HASTEC) CurrencyConverter.hs

# compile the library environment
libraries : vault
	haste-inst install transformers
	cd ./libraries/vault && haste-inst install vault -f-UseGHC
	cd ../reactive-banana && haste-inst install reactive-banana -f-UseGHC
	#$(HASTEC) --libinstall -i./libraries/base/ Data.Unique Control.Monad.ST
	#$(HASTEC) --libinstall -i../reactive-banana/src Reactive.Banana
	#cd ../reactive-banana/ && haste-inst install


# old libraries
#
#libraries : vault libraries/transformers-0.3.0.0
#	$(HASTEC) --libinstall -i./libraries/transformers-0.3.0.0 Control.Monad.Trans.RWS
#	$(HASTEC) --libinstall -i./libraries/vault/src Data.Vault
#	$(HASTEC) --libinstall -i./libraries/base/ Data.Unique Control.Monad.ST
#	$(HASTEC) --libinstall -i./libraries/unordered-containers/ Data.HashMap.Lazy Data.Hashable Data.HashSet
#	$(HASTEC) --libinstall -i../reactive-banana/src Reactive.Banana

#libraries/transformers-0.3.0.0 : 
#	cd libraries && cabal unpack transformers-0.3.0.0

vault : 
	echo "Make sure that the latest git version of the  vault  library is located in  ./libraries/vault"
