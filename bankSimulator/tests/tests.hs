module Main where

import Test.Hspec
import Account
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do
  describe "deposit" $ do
      it "returns new Account details with new balance" $ do
        let acc = BankAccount {accountName = AccountName "John", accountBalance = AccountBalance 100, accountHistory = [Date {debit = Debit 0, credit = Credit 0, balance = AccountBalance 100}]}
        deposit acc 100 `shouldBe` BankAccount {accountName = AccountName "John", accountBalance = AccountBalance 200, accountHistory = [Date {debit = Debit 0, credit = Credit 100, balance = AccountBalance 200},Date {debit = Debit 0, credit = Credit 0, balance = AccountBalance 100}]}
      it "throws an error if no amount is negative" $ do
        let acc = BankAccount {accountName = AccountName "John", accountBalance = AccountBalance 100, accountHistory = [Date {debit = Debit 0, credit = Credit 0, balance = AccountBalance 100}]}
        evaluate (deposit acc (-10)) `shouldThrow` anyErrorCall
  describe "withdraw" $ do
      it "returns new Account details with new balance" $ do
        let acc = BankAccount {accountName = AccountName "John", accountBalance = AccountBalance 100, accountHistory = [Date {debit = Debit 0, credit = Credit 0, balance = AccountBalance 100}]}
        withdraw acc 100 `shouldBe` BankAccount {accountName = AccountName "John", accountBalance = AccountBalance 0, accountHistory = [Date {debit = Debit 100, credit = Credit 0, balance = AccountBalance 0},Date {debit = Debit 0, credit = Credit 0, balance = AccountBalance 100}]}
      it "throws an error if no amount is negative" $ do
        let acc = BankAccount {accountName = AccountName "John", accountBalance = AccountBalance 100, accountHistory = [Date {debit = Debit 0, credit = Credit 0, balance = AccountBalance 100}]}
        evaluate (withdraw acc (-10)) `shouldThrow` anyErrorCall
