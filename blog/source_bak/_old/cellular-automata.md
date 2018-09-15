title: Cellular Automata
date: 2014-04-03 13:44:26
tags: AI
---
## 分形
依然记得高中时第一次用VB绘制曼德勃罗集的情形，其代码本身非常简单，但绘制的图形确异常复杂，甚至让人感到惊叹，这也是第一次直面分形时带来的震撼。
自己高中时还捣鼓过自创的一个图形，其方法和灵感已经不记得了，http://www.fractal.cn/message/read.asp?id=23&subid=1&subtype=%E7%95%99%E8%A8%80%E6%9F%A5%E7%9C%8B 刚才搜索了一下，这个帖子还在，2003年。现在已经是2013年了。 当时非常的挫，只是在论坛上贴了一段vb代码，都没有截图，后来有人运行了我的程序，并给出了截图。

现在看来，整个图形经过反复迭代，呈现了复杂性，当时这个代码是有些模仿曼德勃罗集的，即使反复迭代迭代，然后看迭代之后的值在那个区间范围，然后涂上不同的颜色即可。 当然能产生这样的效果，多少是运气的成分。

## Answer is 42

不管怎么说，这是自己曾经的一种探索。
今天突然又回想起了这些碎片，想到了生命游戏之类的简单却又异常复杂的结构。 特别是想到之前看到了《银河系漫游指南》 其中讲到，the answer to the universe is 42. Why 42? 电影呈现了一个非常搞笑的梗，即使一台能够计算一切的超级人工智能计算机经过计算最后得出了宇宙的终极答案：42。 当时，电影中的所有人都崩溃了，不知道这是什么。 但是计算机很确信，嗯，确实是42.
今天这篇日志，在扯淡的同时想要表达一种思考，answer is 42究竟有没有意义？ 我们从元胞自动机来讲起。

## 元胞自动机

元胞自动机的实现非常简单，整个空间是一个二维网格，每个网格要么是1，要么是0。 一开始第一行除了最中间的网格为1，其余都为0，然后整个生命开始一行一行迭代，每一行的值依赖于上一行的结果。 其迭代规则由一个表格决定：

| current pattern	 	|			111	 | 110	| 101	| 100	| 011	| 010 |	001	 | 000 |
|---|---|---|---|---|---|---|---|---|
| new state for center cell	|	 0	| 0	| 0	 | 1	| 1	| 1	| 1	| 0 |

每一格的值由上一行的三个值来决定，比如表格中上一行相邻三个值为111，则下一行中间那个值即为0. 如果上一行相邻三个值为011，则下一行那个值为1. 整个自动机的规则其实就由第二行的八个bit决定，所以一个元胞自动机可以用一个0-255的整数来表示。

[![例子](/images/f863a7bf19ee143ea0b5c537102e510293b98668.png)](/images/f863a7bf19ee143ea0b5c537102e510293b98668.png)
[![例子](/images/d531240e7f388be5271a7137c94d617a14bebbab.png)](/images/d531240e7f388be5271a7137c94d617a14bebbab.png)
[![例子](/images/f73067411774f9607f0efe6a5b469b1aa98f3ab3.png)](/images/f73067411774f9607f0efe6a5b469b1aa98f3ab3.png)
[![例子](/images/5a1a0c87394719a426b4bffb7e7bb0f3e8608bdd.png)](/images/5a1a0c87394719a426b4bffb7e7bb0f3e8608bdd.png)

上面列举了不同编号的图案，编号为0，15的都比较简单。 编号为18的出现了分形图案，编号为30的最特殊，其分形图案下还有着一定的随机性，即出现了Chaos。 我们可以想想，假设我们在元胞自动机产生的图样中生活，那么不同编号自动机就会产生不同的宇宙，其中有些宇宙比较简单，非常枯燥。 有些宇宙中会产生规则的秩序（例如编号18），有些宇宙中会产生无序，继而产生无穷的复杂性（例如智慧生命，我们所处的宇宙）。 也就是说，尽管这些宇宙的原理是一致的，但是由于内部编码的不同，将会呈现完全不同的面貌。 而我们所处的宇宙，必然不是一个简单枯燥的宇宙（有些宇宙由于规则不好，只有一条直线，或者全部是空白，而我们的宇宙精彩纷呈，对应了编号30）。 所以这就理解了，如果你生活在编号为30的宇宙中，如果存在上帝，上帝会告诉你，the answer to the universe is 30. 因为上帝设计了这个宇宙机器和初始条件，所以答案的意思就是告诉你，你这个宇宙的初始条件，即对应元胞自动机中的初始编码。
最后，再贴几张图：

[![例子](/images/536cb7551d011de3b421cce3e214c1c4200b63f6.png)](/images/536cb7551d011de3b421cce3e214c1c4200b63f6.png)