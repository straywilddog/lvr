# inst/examples/demo_lvr.R
# Demo script for the 'lvr' package
# Author: StrayWildDog
# Description: 演示如何创建lvr对象，模拟轨迹，绘制相空间和极限环

library(lvr)
library(ggplot2)
library(ggquiver)
library(metR)
library(reshape2)

# base --------------------------------------------------------------------

#此节将创建非常简单的线性系统

#定义自我或对方激励函数的函数工厂
lvselF = function(a, b, c = 0) {
  return(function(x, y) {
    a * x + b * y + c * x * y
  })
}
#创建激励函数
f <- lvselF(a = 0, b = 5)
g <- lvselF(a = 0, b = -1.61)

#创建lvr对象
Rlvr = lvrCreate(Name = 'R', Inilv = 0.9,
                 Lvsef = function(x, y) {0}, Lvoth = f) #相当于dR/dt = 0*R + 5*J
Jlvr = lvrCreate(Name = 'J', Inilv = 0.1,
                 Lvsef = function(x, y) {0}, Lvoth = g)

is.lvr(Rlvr)
print.lvr(Rlvr)
print(Jlvr)
Rlvr

Rlvr + 0.2
0.2 + Rlvr
Rlvr - 0.2
1 - Rlvr
Jlvr * 1.1
1 / Jlvr

# example for linear model (LM) -------------------------------------------

#设置步长、步数
step_t = 0.01
nstep = 15000

df = trajlvr(Rlvr, Jlvr, step_t, nstep) #lv为好感度，dl为微分
df = as.data.frame(df)

p = ggplot(df, aes(x = lv1, y = lv2)) + geom_path() +
  annotate("segment", x = df$lv1[nrow(df)], y = df$lv2[nrow(df)],
           xend = df$lv1[nrow(df)] + df$dl1[nrow(df)] * step_t,
           yend = df$lv2[nrow(df)] + df$dl2[nrow(df)] * step_t,
           arrow = arrow(length = unit(.5 , "cm")), color = "red") +
  xlab("R") + ylab("J") +
  theme_bw()
p

# phase space for LM ------------------------------------------------------

limx = c(-7, 7) #相图x轴范围
limy = c(-7, 7)
breaks = 0.5 #坐标轴的间距
step_t = 0.001
nstep = 1

#设置两人的自我和对方激励函数
lvp = list(Lvsef1 = function(x, y) {-0.67 * x}, Lvoth1 = function(x, y) {0.82 * y},
           Lvsef2 = function(x, y) {-0.67 * x}, Lvoth2 = function(x, y) {0.82 * y})

df = phaselvr(xlim = limx, ylim = limy, breaks = breaks, lvparam = lvp,
                 step_t = step_t, Times = nstep)

df = as.data.frame(df)
df = df[which(df$t == 0), ]

p = ggplot(df, aes(x = lv1, y = lv2)) +
  geom_streamline(aes(dx = dl1, dy = dl2), color = "blue", linewidth = .2) +
  scale_x_continuous(limits = limx, expand = c(0, 0)) +
  scale_y_continuous(limits = limy, expand = c(0, 0)) +
  xlab("R") + ylab("J") +
  theme_bw()
p

# example for limit cycle (LR) --------------------------------------------

lvselF = function(a, b) {
  if (b <= 1) {b = 1} else {b = 2}
  return(function(x, y) {
    -x * (sqrt(x ^ 2 + y ^ 2) - a) ^ b #定义极限环
  })
}

f <- lvselF(a = 2, b = 1)
g <- lvselF(a = 1.7, b = 1)

#暖昧期的两人
Rlvr = lvrCreate(Name = 'R', Inilv = 1,
                 Lvsef = f, Lvoth = function(x, y) {1.2 * y})
Jlvr = lvrCreate(Name = 'J', Inilv = 0.5,
                 Lvsef = g, Lvoth = function(x, y) {-1.1 * y})

step_t = 0.01
nstep = 7000

df = trajlvr(Lvr1 = Rlvr, Lvr2 = Jlvr, step_t = step_t, Times = nstep, noise = 0)
df = as.data.frame(df)

p = ggplot(df, aes(x = lv1, y = lv2)) + geom_path() +
  annotate("segment", x = df$lv1[nrow(df)], y = df$lv2[nrow(df)],
           xend = df$lv1[nrow(df)] + df$dl1[nrow(df)] * step_t,
           yend = df$lv2[nrow(df)] + df$dl2[nrow(df)] * step_t,
           arrow = arrow(length = unit(.3 , "cm")),
           color = "red") +
  xlab("R") + ylab("J") +
  theme_bw()
p

step_t = 0.01
nstep = 7050

#添加外界干扰
df = trajlvr(Lvr1 = Rlvr, Lvr2 = Jlvr, step_t = step_t, Times = nstep,
            noise = 1, m = 1, s = 1)
df = reshape2::melt(as.data.frame(df), id.vars = 't', measure.vars = c('lv1', 'lv2'))

p = ggplot(df, aes(x = t, y = value)) +
  geom_path(aes(color = variable)) +
  scale_color_manual(name = ' ',
                     breaks = c('lv1', 'lv2'),
                     values = c("blue", "red"),
                     label = c('R', 'J')) +
  xlab("t") + ylab("Lv") +
  theme_bw()
p #情感变化趋势

#创建激励函数
f <- lvselF(a = 2, b = 2)
g <- lvselF(a = 1.7, b = 2)

#爱情期的两人
Rlvr = lvrCreate(Name = 'R', Inilv = 1,
                 Lvsef = f, Lvoth = function(x, y) {1.2 * y})
Jlvr = lvrCreate(Name = 'J', Inilv = 0.5,
                 Lvsef = g, Lvoth = function(x, y) {-1.1 * y})

step_t = 0.01
nstep = 7000

df = trajlvr(Lvr1 = Rlvr, Lvr2 = Jlvr, step_t = step_t, Times = nstep, noise = 0)
df = as.data.frame(df)

df = reshape2::melt(df, id.vars = 't', measure.vars = c('lv1', 'lv2'))

p = ggplot(df, aes(x = t, y = value)) +
  geom_path(aes(color = variable)) +
  scale_color_manual(name = ' ',
                     breaks = c('lv1', 'lv2'),
                     values = c("blue", "red"),
                     label = c('R', 'J')) +
  xlab("t") + ylab("Lv") +
  theme_bw()
p

# phase space for LR ------------------------------------------------------

# 我们假设两人各自存在自适范围

limx = c(-5, 5)
limy = c(-5, 5)
breaks = 0.2
f <- lvselF(a = 1, b = 1)
g <- lvselF(a = 2.2, b = 1)
#暖昧期的两人
lvp = list(Lvsef1 = f, Lvoth1 = function(x, y) {1.1 * y},
           Lvsef2 = g, Lvoth2 = function(x, y) {-1.2 * y})
step_t = 0.001
nstep = 1

inid = as.data.frame(
  expand.grid(x = seq(limx[1], limx[2], breaks), y = seq(limx[1], limx[2], breaks))
)

inid = as.data.frame(
  expand.grid(x = unique(c(seq(-5, -2, 0.3), seq(-2, 2, 0.15), seq(2, 5, 0.3))),
              y = unique(c(seq(-5, -2, 0.3), seq(-2, 2, 0.15), seq(2, 5, 0.3))))
)

df = phaselvr(data = inid, lvparam = lvp, step_t = step_t, Times = nstep)
df = as.data.frame(df)
df = df[which(as.logical((1 : nrow(df)) %% 2)), ]

p = ggplot(df, aes(x = lv1, y = lv2)) +
  geom_streamline(aes(dx = dl1, dy = dl2), color = "blue", linewidth = .2) +
  scale_x_continuous(limits = limx, expand = c(0, 0)) +
  scale_y_continuous(limits = limy, expand = c(0, 0)) +
  xlab("R") + ylab("J") +
  theme_bw()
p #暖昧期，两人在极限环（彼此的容忍范围）内达到稳态

f <- lvselF(a = 1.5, b = 2)
g <- lvselF(a = 2.2, b = 2)
#爱情期的两人
lvp = list(Lvsef1 = f, Lvoth1 = function(x, y) {2.1 * y},
           Lvsef2 = g, Lvoth2 = function(x, y) {-1.2 * y})

step_t = 0.001
nstep = 1

df = phaselvr(data = inid, lvparam = lvp, step_t = step_t, Times = nstep)
df = as.data.frame(df)
df = df[which(as.logical((1 : nrow(df)) %% 2)), ]

p = ggplot(df, aes(x = lv1, y = lv2)) +
  geom_streamline(aes(dx = dl1, dy = dl2), color = "blue", linewidth = .2) +
  scale_x_continuous(limits = limx, expand = c(0, 0)) +
  scale_y_continuous(limits = limy, expand = c(0, 0)) +
  xlab("R") + ylab("J") +
  theme_bw()
p #爱情期，两人的极限环消失，终将回到原点

# simple game for LR ------------------------------------------------------

# 认为对方喜欢自己而选择表白的概率
pxyes <- function(a) {
  return(function(x, y) {
    if (length(x) != length(y)) stop("x and y must be of the same length")
    ifelse(y < 0, 0, ifelse(y < a, y / a, 1))
  })
}
# 当对方好感度达到了 a 后
# 必然会选择表白
# 如果对方有 px 的概率选择表白
# 那么向对方表白也会有 px 的概率被同意
# 如果自身有 px 的概率选择表白
# 那么对方表白时也会有 px 的概率同意

# 选择 3.1.1 中的模型
f <- function(a, b) {
  if (b <= 1) {b = 1} else {b = 2}
  return(function(x, y) {
    -x * (sqrt(x ^ 2 + y ^ 2) - a) ^ b #定义极限环
  })
}
g <- function(d, X) {
  pxf = X$pac
  sm = X$score
  return(function(x, y) {
    px = pxf(x, y) #计算选择表白的概率
    d * y + ev_Yeffect(pX = px, s = sm, pY = pxyes(2)(x, y), response = F) #对方选择不告白的纯策略
  })
}

a1 = 1.9
a2 = 2.4
b = 1
d1 = -1.1
d2 = 1.5

Rlvr = lvrCreate(Name = "Romeo", Inilv = 0.9, P = 0.5,
                 Lvsef = f(a = a1, b = b), Lvoth = g(d1, Jlvr),
                 Pac = pxyes(2),
                 Score = c(7, -3, -5, 1))
Jlvr = lvrCreate(Name = "Juliet", Inilv = 0.1, P = 0.5,
                 Pac = pxyes(2),
                 Lvsef = f(a = a2, b = b), Lvoth = g(d2, Rlvr),
                 Score = c(7, -3, -5, 1))

lvp = list(Lvsef1 = f(a = a1, b = b), Lvoth1 = g(d1, Jlvr),
           Lvsef2 = f(a = a2, b = b), Lvoth2 = g(d2, Rlvr))

inid = expand.grid(x = seq(-3, 3, .1),
                   y = seq(-3, 3, .1))
df = as.data.frame(phaselvr(data = inid, lvparam = lvp, step_t = 0.01, Times = 1))
df = df[which(df$t == 0), ]

p = ggplot(df, aes(x = lv1, y = lv2)) +
  geom_streamline(aes(dx = dl1, dy = dl2), color = "blue", linewidth = .2,
                  arrow.angle = 15) +
  scale_x_continuous(limits = c(min(inid[, 1]), max(inid[, 1])), expand = c(0, 0)) +
  scale_y_continuous(limits = c(min(inid[, 2]), max(inid[, 2])), expand = c(0, 0)) +
  xlab("R") + ylab("J") +
  theme_bw()
p

# END ---------------------------------------------------------------------



#最后祝有缘人终成眷属
