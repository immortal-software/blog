title: Monad
date: 2014-05-07 18:27:25
tags:
---

这个系列的第二篇文章，上次讨论了Pearls of Functional Algorithm Design这本书以及他的第一个问题，本来打算继续介绍第二个问题，但是发现前几个问题都是Divide And Conquer的思路，有些重复，所以这次先聊些其他的有趣的问题。

最近东看西看，http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf 这是一篇很有意思的文章，介绍概率分布、概率计算在Hask中的实现。 http://slawekk.wordpress.com/2009/05/31/probability-monad/ 提到了概率分布可以是Monad的一个实例，从中体现Monad的作用。消化之后我实现了一个非常简单的demo来说明其思想，暂不考虑运行效率，话不多说，先放代码：

```haskell
data Dist a = Dist { unD :: [(a, Double)] } deriving (Show, Eq, Read)
type Event a = (a -> Bool)

-- get a uniform distribution from list of element
uniform :: [a] -> Dist a
uniform xs = Dist (map (\a -> (a, 1.0 / l)) xs) where
    l = fromIntegral $ length xs

-- get event happen probability
getEventProb :: (Dist a) -> (Event a) -> Double
getEventProb d event = (sum . (map snd) . filter (event . fst) . unD $ d) / 
    (sum . (map snd) . unD $ d)

-- normalize a distribution
normalize :: (Ord a, Eq a) => (Dist a) -> (Dist a)
normalize d = Dist [(x, getEventProb d (==x)) | x <- (nub . sort . map fst) $ unD d]

-- monad
instance Monad Dist where
    return x = Dist [(x, 1)]
    (Dist d) >>= f = Dist [ (y, p * q) | (x, p) <- d, (y, q) <- unD (f x) ]

-- move function, transition function
move :: String -> Dist String
move "Head" = uniform ["Head", "Tail"]
move "Tail" = uniform ["Tail"]

-- example
move2 = uniform ["Head"] >>= move >>= move
answer = normalize move2

main = print answer
```
 
[1 of 1] Compiling Main             ( prob.hs, prob.o )
Linking prob ...
Dist {unD = [("Head",0.25),("Tail",0.75)]}

#### 随机变量的取值分布
我们来逐行分析，首先，定义了data Dist，就是概率分布的一个数据结构，我们用(a = value, Double = prob)，表示在此概率分布上，取值为a这个类型，随机变量X = value, 对应的概率为prob。 例如，(“Head”, 0.5)表示某个随机变量=”Head”的概率为0.5。 基于此，概率分布（即随机变量）= [(a, Double)]，即一系列可能取值的集合。 例如X = [("Head", 0.5), ("Tail", 0.5)]就表示掷硬币的结果分布。

#### 产生均一分布
接下来，我们定义了uniform函数，根据一个列表来产生一个均一分布。 假设列表长度为n，则每个值的概率都是1/n。这个比较直接。

#### 如何表示事件
另外定义Event type，就是别名，我们认为(a->Bool)的一个函数为一个事件。

#### 如何计算事件发生的概率
接下来，我们可以定义事件发生的概率如何计算，getEventProb函数就是把所有在事件上为真的项的概率加起来。 这里就体现为何我们用(a->Bool)来定义事件了，根据概率的定义，一个事件就是所有可能项的一个子集。 例如考试成绩的取值为[0,100]中的整数， 我们定义及格这个事件就是分数取值的一个子集[60,100]， 用子集来定义事件太麻烦，需要一个一个列出来。 进一步，我们可以用一个二值函数来刻画一个子集，这个二值函数返回true，则表示某项属于这个事件，反之则不属于。
例如[60,100] = filter (>=60) [0..100]。 (>=60)就定义了一个事件。

normalize函数很直接，就是对概率归一化。 因为[(a, Double)]中会出现重复的a，所以这里会对项进行排序和去重，使得normalize之后会合并同类项。

#### 概率转移
接下来，我们引入概率转移，在随机过程中，我们通常会定义转移矩阵，即某值在下一回合（文明打多了。。）变成另外一个值的概率已知。例如在例子中，状态为["Head", "Tail"]，我们有转移函数move，并且我们定义如果当前是”Head”，则再投掷一个次硬币，则下回合可能变成Head或者Tail, 如果当前是Tail，则不投了，一直保持Tail。 我们想知道，对于初始概率分布[("Head", 1)]，即一开始状态为Head，运行两个回合之后结果是Tail的概率。
抽象来说，即我们当前有个分布Dist a，我们对每个值x，都有一个他转移之后的分布，即我们有函数(a -> Dist b)。 现在问题是，假设我们有Dist a, 和函数(a -> Dist b)，如何得到 Dist b。
这个时候，我们就发现，这个和Monad的约束是一致的。

#### map, fmap, applicative functor, monad
我们对比下四者：map, fmap, applicative functor, monad,

* map:: (a->b) -> [a] -> [b]
给定(a->b)的一个函数，和a类型的列表，返回b类型的列表

* fmap在map上进行了抽象
fmap:: (a->b) -> f a -> f b
给定(a->b)的一个函数，和一盒子类型a，返回一盒子类型b（盒子表示一个抽象结构，列表是其特例）

* applicative functor :: (f (a->b)) -> f a -> f b
给定在盒子中进行(a->b)的函数，和一盒子类型a，返回一盒子类型b

* monad:: m a -> (a -> m b) -> m b
给定一盒子类型a，给定从a变成一盒子b的函数，返回一盒子b。
按道理来说，给你一盒子a，每个a可以变成一盒子b，那么结果应该是一盒子包含一盒子b的东西，
从直觉上来说即[a] -> (a -> [b]) -> [[b]]，这是直觉的。
如果我们需要[a] -> (a -> [b]) -> [b]，即意味着我们需要实现一种（递归的结构）的塌缩方法。
即从[[b]] -> [b]的方法，这就是我理解的Monad所体现的一种操作规范。即结构的结构塌缩成结构本身的方法。


回到例子中，(Dist a) -> (a -> Dist b) -> Dist b 就体现了概率转移一次之后的结果。 即从当前概率分布，经过一个概率转移函数，变成了下一个概率分布。 而概率转移函数是定义在a类型上的，而不是定义在Dist a这个类型上，这非常符合数学。 如果去定义（Dist a -> Dist b)的函数作为转移函数，其实就给自己找麻烦。
另外通常情况下a, b是同一类型。
这样，最终我们就可以使用 uniform ["Head"] >>= move >>= move 这样简洁漂亮的写法来实现对此随机过程的模拟。
呼。