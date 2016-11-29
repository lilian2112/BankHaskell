module Account where

newtype AccountName = AccountName String
  deriving (Eq,Show)
newtype AccountBalance = AccountBalance Int
  deriving (Eq,Show)
newtype Debit = Debit Int
  deriving (Eq,Show)
newtype Credit = Credit Int
  deriving (Eq,Show)

data Date = Date String
  deriving (Eq,Show)

data AccountHistory = Activity
  { date :: Date
  , debit :: Debit
  , credit :: Credit
  , balance :: AccountBalance
  } deriving (Eq,Show)

data BankAccount = BankAccount
  { accountName   :: AccountName
  , accountBalance :: AccountBalance
  , accountHistory :: [AccountHistory]
  } deriving (Eq,Show)

addHistory :: [AccountHistory] -> AccountHistory -> [AccountHistory]
addHistory oldHistory addHistoryItem = (addHistoryItem : oldHistory)

deposit :: BankAccount -> Int -> String -> BankAccount
deposit account amount dateInput
   | amount <= 0 = error $ "You cannot deposit negative amount"
   | otherwise = changedAccount
        where currentBalance = case (accountBalance account) of
                                  AccountBalance balance -> balance
              newBalance     = AccountBalance (currentBalance + amount)
              addHistoryItem     = Activity {date = Date dateInput, debit = Debit 0, credit = Credit amount, balance = newBalance}
              newHistory     = addHistory (accountHistory account) addHistoryItem
              changedAccount = BankAccount (accountName account) newBalance newHistory

withdraw :: BankAccount -> Int -> String -> BankAccount
withdraw account amount dateInput
   | amount <= 0 = error $ "You cannot withdraw a negative amount"
   | otherwise = changedAccount
        where currentBalance = case (accountBalance account) of
                                  AccountBalance balance -> balance
              newBalance     = AccountBalance (currentBalance - amount)
              addHistoryItem     = Activity {date = Date dateInput, debit = Debit amount, credit = Credit 0, balance = newBalance}
              newHistory     = addHistory (accountHistory account) addHistoryItem
              changedAccount = BankAccount (accountName account) newBalance newHistory
