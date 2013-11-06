HIT 《数据结构与算法分析》 Scheme解法
===========================

# 把戏、杂技和先辈的括号——献给住在计算机内的神灵们

数学家们用奇怪的符号组成美妙的式子来表达具有深刻逻辑性的思想。

工程师们不但有从数学家那里继承来的严谨形式化体系，他们还有……呃，`NULL`和指针！

图灵机和λ-演算，前者在冯·诺依曼体系的补充下显得更加地“工程”，而后者却更加地“数学”。作为两种模型体现，C语言和Scheme（Lisp）完全就像是两种极端：那些说C语言的人，他们甚至能看到新鲜的二进制位，他们还通常拿着大刀在这些比特上砍砍削削；而那些满嘴Lisp方言的人，他们似乎住在一个耸立天际的高塔中，他们会建立各种各样的抽象层，将一些底层的思想给隔离开来。呵！那些住在塔顶层的人们，似乎还没发觉这座塔是由他们之中那些能工巧匠“凭空”建造的！

在C语言或者是Java语言的程序员眼中，Scheme（Lisp）程序员总像是一个咏唱远古咒语的巫师。但更多的时候，这些巫师更像是马戏团的抛球小丑：他们总善于将大量的括号抛起在空中，又准确地接住。不要奇怪这些括号中会混入像`lambda`和`eval-apply`这样的诡秘东西——这也是他们善用的把戏！

Scheme（Lisp）远比C语言要亲和数据结构：广义表（S-表达式）已经作为Scheme（Lisp）的语法基石深深地奠基在语言核心中，通过使用`cons`，我们可以将原子构建成表，甚至可以将原子和表组成新表！我们将看到……凡是C语言能描述的数据结构，Scheme（Lisp）都能够模拟！

我们的雄心，并非需要`malloc()`才能开辟。保持一颗对数学虔诚探索的心，一种对工程谨严思辨的精神，我们将开始用远古的咒语来颂唱不太久远的诗篇。并谨以此献给那些住在计算机内的神灵们！

# 这是对你的启发，而非对你的限制

如果我的小学弟（或者是可爱的小学妹）很不幸地“搜索”到了这个Scheme解法，请不要懊恼，虽然它不能帮助你“快速地”完成令你烦恼的数据结构的作业，但它会给你洞开一扇通向奇异世界的大门。请插上想象的翅膀，摆脱C语言的束缚，在数据结构的世界里自由翱翔！

如果你因此对Scheme产生了兴趣，你可以参考[Scheme新手教程](http://deathking.github.io/yast-cn/)，该教程也像本项目一样托管在Github上，它在[yast-cn](https://github.com/DeathKing/yast-cn)这个仓库中。

同时，计算机界里有一本非常优秀的教科书——[《计算机程序的构造和解释》](http://mitpress.mit.edu/sicp/)，它也是用Scheme来授课的。你可以在[Learning-SICP](https://github.com/FoOTOo/Learning-SICP)这个仓库中取得公开课字幕的资源。

# 参考文献

下面陈列的文献都是学习Scheme（Lisp）很好的资料。读者可以从这些文献的出版日期从感受到厚重的历史底蕴。

+ Allen J. Anatomy of LISP[M]. McGraw-Hill, Inc., 1978.
+ Siklossy L. Let's talk LISP[M]. Englewood Cliffs, NJ: Prentice-Hall, 1976.
+ Sussman G, Abelson H, Sussman J. Structure and interpretation of computer programs[J]. The Massachusetts Institute of Technology, 1985, 10.
+ Friedman D P. The Little Schemer[M]. The MIT Press, 1996.

# 联系我

如果您认为我的解法有错误或者过于笨拙，我欢饮您提交Pull Request。

如果您想与我交流，欢迎致信`dk[at]hit.edu.cn`。
