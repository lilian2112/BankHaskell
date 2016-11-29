module Main where

import Test.Hspec
import Account
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do
  describe "deposit" $ do
      it "returns new Account details with new balance" $ do
        let acc = BankAccount {accountName = AccountName "John", accountBalance = AccountBalance 100, accountHistory = [Activity {date = Date "12/12/1212", debit = Debit 0, credit = Credit 0, balance = AccountBalance 100}]}
        deposit acc 100 "12/21/2016" `shouldBe` BankAccount {accountName = AccountName "John", accountBalance = AccountBalance 200, accountHistory = [Activity {date = Date "12/21/2016", debit = Debit 0, credit = Credit 100, balance = AccountBalance 200}, Activity {date = Date "12/12/1212", debit = Debit 0, credit = Credit 0, balance = AccountBalance 100}]}
      it "throws an error if no amount is negative" $ do
        let acc = BankAccount {accountName = AccountName "John", accountBalance = AccountBalance 100, accountHistory = [Activity {date = Date "12/12/1212", debit = Debit 0, credit = Credit 0, balance = AccountBalance 100}]}
        evaluate (deposit acc (-10)"12/21/2016") `shouldThrow` anyErrorCall
  describe "withdraw" $ do
      it "returns new Account details with new balance" $ do
        let acc = BankAccount {accountName = AccountName "John", accountBalance = AccountBalance 100, accountHistory = [Activity {date = Date "12/12/1212", debit = Debit 0, credit = Credit 0, balance = AccountBalance 100}]}
        withdraw acc 100 "12/21/2016" `shouldBe` BankAccount {accountName = AccountName "John", accountBalance = AccountBalance 0, accountHistory = [Activity {date = Date "12/21/2016", debit = Debit 100, credit = Credit 0, balance = AccountBalance 0},  Activity {date = Date "12/12/1212", debit = Debit 0, credit = Credit 0, balance = AccountBalance 100}]}
      it "throws an error if no amount is negative" $ do
        let acc = BankAccount {accountName = AccountName "John", accountBalance = AccountBalance 100, accountHistory = [Activity {date = Date "12/12/1212", debit = Debit 0, credit = Credit 0, balance = AccountBalance 100}]}
        evaluate (withdraw acc (-10)"12/21/2016") `shouldThrow` anyErrorCall
