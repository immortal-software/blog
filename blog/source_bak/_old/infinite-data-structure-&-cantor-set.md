title: Infinite Data Structure & Cantor Set
date: 2014-04-03 18:27:04
tags:
---

[![Cantor Set](/images/f9b8da50356299117a6fbd4adf2f843947bf3da2.png)]

记得大学时邹恒明老师曾把操作系统比喻为一名魔术师。 因为他所做的工作，就是掩盖底层物理设备的不完美与不和谐，而对运行其上的程序提供一个舒适的家园。 每个进程都有自己的超大的地址空间，CPU仿佛一直在为其不停的计算，各个设备都能够友善的访问和操作。 这些都不是凡人能做到的。 操作系统对进程来说，就是MATRIX，就是为进程们编制了一个完美的虚拟世界。
所以无论是此时正在写博客的我，亦或是读者，都应该感谢操作系统。
然而，本文不是要歌颂或是讨论操作系统，只是引申开来，讲一些与之联系的思考。 这里我们要讨论的是处理无穷大的能力。 直截了当的说就是，函数式语言能够很好的刻画递归，此外数据结构本身也可以递归的定义，如果此时，这个函数式语言还支持Lazy Evaluation的特性，那么就为上层编程者提供了一种处理无穷的能力。
即你可以通过定义一个无限递归的数据结构，并对他进行运算。
举个例子来说，康托三分集是一个非常著名的分形结构。 由德国数学家康托与1883年引入，康托集构造很简单，初始为[0,1]的线段，然后每次对每个线段，从中去除中间三分之一的开区间，对左右两边三分之一各自递归直至无穷。 本文最上的图示就展示了康托集合每一步的划分。
康托集合有如下性质：
* 一、包含无穷多个点
* 二、不包含任何非零长度的线段
* 三、总长度为0


我们可以通过HASKELL用以下方式来定义康托集：

```haskell
import Data.Ratio
data Cantor a = Vacant | Interval a a (Cantor a) (Cantor a) (Cantor a)
getL (Interval a _ _ _ _) = a
getR (Interval _ a _ _ _) = a
 
buildCantor :: Ratio Integer -> Ratio Integer -> Cantor (Ratio Integer)
buildCantor l r = (Interval l r) (buildCantor l ((r - l) / 3 + l)) (Vacant) (buildCantor (r - (r - l) / 3) r)
 
inCantor :: (Ord a) => (Cantor a) -> a -> Bool
inCantor Vacant _ = False
inCantor (Interval a b t1 t2 t3) (x) = (x >= a && x <= b)
 
findNumber :: Cantor (Ratio Integer) -> Ratio Integer -> String 
findNumber (Interval a b t1 t2 t3) x 
    | (inCantor t1 x) = "[" ++ (show a) ++ "," ++ (show $ getR t1) ++ "]" ++ "=>" ++ (findNumber t1 x)
    | (inCantor t3 x) = "[" ++ (show $ getL t3) ++ "," ++ (show b) ++ "]" ++ "=>" ++ (findNumber t3 x)
    | otherwise = "[" ++ (show $ getR t1) ++ "," ++ (show $ getL t3) ++ "]" ++ "=>" ++ (findNumber t2 x) 
findNumber Vacant x = "Bingo!"
 
main = print (findNumber (buildCantor 0 1) (1 % 10))
```

首先定义数据结构Cantor，他本身要么是个空集，要么是左中右三段。
buildCantor函数构造了一个无限的康托集，我们使用Ratio Integer来表示有理数，他可以支持任意高精度。
接下来，我们实现findNumber函数在这个无限的结构中寻找一个点是否存在，如果一个点在某一层落入了Vacant，则他不在康托集中，那么我们可以得到输出其路径。
很多数都会很快Bingo.
例如：输入为12/17,输出为
“[2 % 3,1 % 1]=>[2 % 3,7 % 9]=>[19 % 27,20 % 27]=>Bingo!”
输入为2/5，则
“[1 % 3,2 % 3]=>Bingo!”
输入为1/10，看似简单，但
“[0 % 1,1 % 3]=>[0 % 1,1 % 9]=>[2 % 27,1 % 9]=>[8 % 81,1 % 9]=>[8 % 81,25 % 243]=>[8 % 81,73 % 729]=>[218 % 2187,73 % 729]=>[656 % 6561,73 % 729]=>[656 % 6561,1969 % 19683]=>[656 % 6561,5905 % 59049]。。。。。。
程序在内存耗尽前将永远无法结束，这个点存在于康托集中，但永远找不到他属于那个区间，因为康托集不包含任何非零长度的线段。
这个程序的魅力在于我们有能力去刻画这个无限集合，同时，在构造时，不需要事先指定构造层数之类的细节。 由于lazy特性，只要当需要时，我们才会去展开需要的分支，所以无需担心无限的结构一开始耗尽内存。 这个判断程序一个while循环在任何语言中都能实现，但这样的实现方式还是有其独特意义的，因为我们首先定义并构造了这个无限集合，之后我们再对齐运算。 而while的判断实现实际上并没有得到这个集合。