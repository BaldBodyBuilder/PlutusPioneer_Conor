module Test where
{-
1st major change is in the mkValidator function
In the previous version the 3rd arguement was called validator CTX 
-}
mkValidator :: () -> MySillyRedeemer -> ScriptContext -> Bool
mkValidator = () (MySillyRedeemer r) _ = traceIfFalse "wrong redeemer" $ r == 42

{- The second major change is where we create the scrAddress-}
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
{-Previously scrAddress was created using ScriptAddress passing in a validator hash.
This is because the address type has now changed in order to allow a component of thea ddress realting to the staking address
But there is still a smart constructor ~scriptAddress~ 
We don't need the validator hash anymore
-}

{-Fees are now considered in the playground, always 10 lovelace-}

{-In order to unlock a script address the script attached to the address is run and that script get 3 pieces of information, the
datum, redeemer and context
Generally Datum and Redeemer are custom Types
Focus of week 3 is context
-}

{-
Script Context is dfined in package plutus-ledger-api which is a package that we haven't really needed
But now we do need it and its defined in |Plutus.V1.Ledger.Contexts|
-}

data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }

{-It is a record type, haskell type in which the fields are given names rather than being referred to only by their position/type
Second field is of type ScriptPurpose which is defined in the same module it defines for which purpose a script is being run
-}

data ScriptPurpose
    = Minting CurrencySymbol
    | Spending TxOutRef
    | Rewarding StakingCredential
    | Certifying DCert

{-
Most important is spending, this is when a script is run in order to validate a spending input for a transaction
the minintg purpose comes into play when you want to define a native token
Its purpose is to describe under which circumstances the native token can be minted or burned.
There are also two brand new purposes, Rewarding (Staking), and Certifying (Stake Delegation)
scrtipContextTxInfo is the most interesting and is of type TxInfo it describes the spending TX
-}

-- | A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.
data TxInfo = TxInfo
   { txInfoInputs      :: [TxInInfo] -- ^ Transaction inputs
   , txInfoInputsFees  :: [TxInInfo]     -- ^ Transaction inputs designated to pay fees
   , txInfoOutputs     :: [TxOut] -- ^ Transaction outputs
   , txInfoFee         :: Value -- ^ The fee paid by this transaction.
   , txInfoForge       :: Value -- ^ The 'Value' forged by this transaction.
   , txInfoDCert       :: [DCert] -- ^ Digests of certificates included in this transaction
   , txInfoWdrl        :: [(StakingCredential, Integer)] -- ^ Withdrawals
   , txInfoValidRange  :: SlotRange -- ^ The valid range for the transaction.
   , txInfoSignatories :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
   , txInfoData        :: [(DatumHash, Datum)]
   , txInfoId          :: TxId
   -- ^ Hash of the pending transaction (excluding witnesses)
   } deriving (Generic)
{-The context of validation is the spending transaction and its inputs and outputs
List of all inputs is txInfoInputs and the list of all outputs is txInfoOutputs
Fields for fees txFee and the forge value of txInfoForge used when minting or burning native tokens
the field txInfoValidRange which defines the slot range for which this transaction is valid
txInfoData is a list associating datums with their respective hashes, if there is a tx output to a script address that carries over sume datum
    we don't have to include the datum we can just include the datum hash but we can optionally attach the datum in which case it will be done in txInfoData list
txInfoId field is a hash of the transaction including all its inputs and outputs.
-}

{-
txInfoValidRange
Cardano's validation can happen in the wallet
TXs can still fail on chain
following validation if, when the TX arrives on the blockchain the SC has already been consumed by someone else in this case the tx fails and no fees are paid
But how do we manage time?
With the auction example we allowed bids until the deadline had been reached and the close endpoint can only be called after the deadline has been passed
Cardano solves the determinism dilemna by adding the txInfoValidRange to a tx which essentially says this tx is valid between x and y slot
One check in the validation phase is the checking if the slot range is valid
by default a script uses the infinite slot range, one that covers all slots until the end of time
but we do have the option to set different slot ranges 
-}

{-
Slots
Plutus.V1.Ledger.Slot

-- | The slot number. This is a good proxy for time, since on the Cardano blockchain
-- slots pass at a constant rate.
newtype Slot = Slot { getSlot :: Integer }
   deriving stock (Haskell.Eq, Haskell.Ord, Show, Generic)
   deriving anyclass (FromJSON, FromJSONKey, ToJSON, ToJSONKey, NFData)
   deriving newtype (Haskell.Num, AdditiveSemigroup, AdditiveMonoid, AdditiveGroup, Enum, Eq, Ord, Real, Integral, Serialise, Hashable, PlutusTx.IsData)


Can use the num class so we could write "Slot 17" or {getSlot=17}
definition of Slot range is:
-- | An 'Interval' of 'Slot's.
type SlotRange = Interval Slot

What is interval though?
--   The interval can also be unbounded on either side.
data Interval a = Interval { ivFrom :: LowerBound a, ivTo :: UpperBound a }
   deriving stock (Haskell.Eq, Haskell.Ord, Show, Generic)
   deriving anyclass (FromJSON, ToJSON, Serialise, Hashable, NFData)

Bounds are inclusive, could specify if the lower is beginning of time and the upper is infinity

Upper and lower are inclusive:
interval :: a -> a -> Interval a
interval s s' = Interval (lowerBound s) (upperBound s')

Singleton helper
singleton :: a -> Interval a 
singleton s = interval s s

We haave 'from' which constructs an 'Interval' starting from a given slot and extending to the end of time.
    from :: a -> Interval a
    from s = Interval (lowerbound s) (UpperBound PosInf True)
gensis block up to and including the given slot
    to :: a -> Interval a
    to s = Interval (LowerBound NegInf True) (upperBound s)
From beginning of time to eternity
    always :: Interval a
    always = Interval (LowerBound NegInf True) (UpperBound PosInf True)
and the opposite 'never' which contains no slots
    never :: Interval a
    never = Interval (LowerBound PosInf True) (UpperBound NegInf True)

Helpers for working with intervals

member function checks whether a value is contained within an Interval
    member :: Ord a => a -> Interval a -> Bool
    member a i = i `contains` singleton a

Overlaps function checks whether two intervals overlap,that is, whether there is a value that is a member of both intervals
    overlaps :: Ord a => Interval a -> Interval a -> Bool
    overlaps l r = isEmpty ( l `intersection` r)

Intersection functions deteremines the largest interval that is contained in both the given intervals 
    hull :: Ord a => Interval a -> Interval a -> Interval a
    hull (Interval l1 h1) (Interval l2 h2) = Interval (min l1 l2) (max h1 h2)

Contains takes two intervals and determines if the second interval is completely contained within the first
contains :: Ord a => Interval a -> Interval a -> Bool
contains (Interval l1 h1) (Interval l2 h2) = l1 <= l2 && h2 <= h1

And before or after functions to determine slot time location
before :: Ord a => a -> Interval a -> Bool
before h (Interval f _) = lowerBound h < f

after :: Ord a => a -> Interval a -> Bool
after h (Interval _ t) = upperBound h > t

-}

{-
Playing in the repl

-}
