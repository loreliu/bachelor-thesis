# 本科学士学位论文《线性判别分析的原理与应用》用到的代码

# 环境信息
if (!require(xfun)) install.packages('xfun')
xfun::session_info(packages = c('GGally', 'ggplot2', 'dplyr', 'tidyr', 'forcats', 'viridis', 'mvnormtest', 'MVTests',
    'MASS', 'klaR'), dependencies = FALSE)
# R version 4.1.3 (2022-03-10)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 20.04.4 LTS, RStudio 0

# Locale:
#   LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8       
#   LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
#   LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
#   LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

# Package version:
#   dplyr_1.0.9      forcats_0.5.1    GGally_2.1.2     ggplot2_3.3.6   
#   klaR_1.7-0       MASS_7.3-56      mvnormtest_0.1-9 MVTests_2.1.1   
#   tidyr_1.2.0      viridis_0.6.2


# 加载相关软件包，如果没有安装会自动安装
xfun::pkg_attach2(c('GGally', 'ggplot2', 'dplyr', 'tidyr', 'forcats', 'viridis'))


timeline_840 = read.csv('https://raw.githubusercontent.com/loreliu/bachelor-thesis/main/timeline_840.csv', header = TRUE,
    encoding = 'UTF-8')
# timeline_840 = read.csv('timeline_840.csv', header = TRUE, encoding = 'UTF-8')

# 因子化 winningTeam
timeline_840$winningTeam = as.factor(timeline_840$winningTeam)

summary(timeline_840)

# 分组 summary
summary_blue_red = by(timeline_840[, 1:15], timeline_840$winningTeam, summary)
summary_blue_red

# 分组 std
timeline_840_winningTeam_blue = timeline_840$winningTeam == 100
sd_winblue = lapply(timeline_840[timeline_840_winningTeam_blue, ][, 1:15], sd)
sd_winred = lapply(timeline_840[!timeline_840_winningTeam_blue, ][, 1:15], sd)
sd_winblue
sd_winred


attach(timeline_840)
dragonDiff = t1Dragons - t2Dragons
riftDiff = t1Rift - t2Rift
topTowerTakenDiff = t1TopTowerTaken - t2TopTowerTaken
midTowerTakenDiff = t1MidTowerTaken - t2MidTowerTaken
botTowerTakenDiff = t1BotTowerTaken - t2BotTowerTaken
summary(cbind(dragonDiff, riftDiff, topTowerTakenDiff, midTowerTakenDiff, botTowerTakenDiff))
timeline_840_Diff = cbind(timeline_840, cbind(dragonDiff, riftDiff, topTowerTakenDiff, midTowerTakenDiff,
    botTowerTakenDiff))

# 分组 diff mean
mean_diff_winblue = lapply(timeline_840_Diff[timeline_840_winningTeam_blue, ][, 28:32], mean)
mean_diff_winred = lapply(timeline_840_Diff[!timeline_840_winningTeam_blue, ][, 28:32], mean)
mean_diff_winblue
mean_diff_winred

# 分组 diff std
sd_diff_winblue = lapply(timeline_840_Diff[timeline_840_winningTeam_blue, ][, 28:32], sd)
sd_diff_winred = lapply(timeline_840_Diff[!timeline_840_winningTeam_blue, ][, 28:32], sd)
sd_diff_winblue
sd_diff_winblue


# 各变量的相关系数矩阵
ggcorr(timeline_840[, 1:15], method = c('pairwise', 'pearson'), label = TRUE, label_alpha = TRUE,
    label_round = 4, hjust = 0.5, name = '各变量的相关系数矩阵')

# 各位置的分组对位经济差散点图矩阵
pairs_diff_gold = ggpairs(timeline_840[, 11:15], ggplot2::aes(color = winningTeam, alpha = 0.5),
    upper = list(continuous = wrap('cor', size = 2.5)),
    lower = list(continuous = 'smooth'), title = '各位置的分组对位经济差散点图矩阵')
# 修改调色板 100：blue 200：red
for (i in 1:pairs_diff_gold$nrow) {
    for (j in 1:pairs_diff_gold$ncol) {
        pairs_diff_gold[i, j] = pairs_diff_gold[i, j] +
            scale_fill_manual(values = c('blue', 'red')) +
            scale_color_manual(values = c('blue', 'red'))
    }
}
pairs_diff_gold


# 对位经济领先胜率
win_perc_gold = function(start, end, interval, columns) {
    n = floor((end - start) / interval) # 区间数
    df_prec_gold = data.frame() # 存储计算结果
    intervals = c() # 区间名
    for (i in 1:length(columns)) {
        for (j in 0:n) {
            if (j < n) {
                # 区间名
                if (i == 1) {
                    intervals[length(intervals) + 1] = paste(start + interval * j, '-', start + interval * (j + 1))
                }
                # 计算胜率
                tl_subset = timeline_840[start + interval * j <= timeline_840[, columns[i]] & timeline_840[, columns[i]] <
                    start + interval * (j + 1), ]
                prec = sum(tl_subset$winningTeam == 100) / nrow(tl_subset)
                df_prec_gold[i, (j + 1)] = prec
            } else {
                # 区间名
                if (i == 1) {
                    intervals[length(intervals) + 1] = paste('>', start + interval * j)
                }
                # 计算胜率
                tl_subset = timeline_840[start + interval * j <= timeline_840[, columns[i]], ]
                prec = sum(tl_subset$winningTeam == 100) / nrow(tl_subset)
                df_prec_gold[i, (j + 1)] = prec
            }
        }
    }
    rownames(df_prec_gold) = columns
    colnames(df_prec_gold) = intervals
    win_perc_list = list('intervals' = intervals, 'df' = df_prec_gold)
    return(win_perc_list)
}
win_perc_list = win_perc_gold(0, 4500, 500, colnames(timeline_840)[11:15])
win_perc_list


# 对位经济差折线图
win_perc_df = data.frame(gold = rep(seq(0, 4500, 500), rep(5, 10)),
    win_perc = unname(unlist(win_perc_list$df)),
    role = factor(rep(rownames(win_perc_list$df), 5), levels = rownames(win_perc_list$df)))

ggplot(win_perc_df, aes(x = gold, y = win_perc, group = role, color = role)) +
    geom_line() +
    scale_color_manual(labels = c('上单', '打野', '中单', 'ADC', '辅助'), values = c('blue', 'red',
        'magenta', 'green', 'black')) +
    labs(color = '位置') +
    xlab('14分钟时蓝色方各位置的对位经济领先') +
    ylab('蓝色方各位置的获胜概率') +
    ggtitle('14分钟时蓝色方各位置对位经济领先的胜率')


# 蓝色方夺取小龙胜率
df_prec_dragons = data.frame() # 存储计算结果
for (i in 0:2) {
    for (j in 0:2) {
        # 计算胜率
        tl_subset = timeline_840[timeline_840$t1Dragons == i & timeline_840$t2Dragons == j, ]
        prec = sum(tl_subset$winningTeam == 100) / nrow(tl_subset)
        df_prec_dragons[i + 1, j + 1] = prec
    }
}
rownames(df_prec_dragons) = c('blue_team_0', 'blue_team_1', 'blue_team_2')
colnames(df_prec_dragons) = c('red_team_0', 'red_team_1', 'red_team_2')
df_prec_dragons


# 蓝色方夺取峡谷先锋胜率
df_prec_rift = data.frame() # 存储计算结果
for (i in 0:1) {
    for (j in 0:1) {
        # 计算胜率
        tl_subset = timeline_840[timeline_840$t1Rift == i & timeline_840$t2Rift == j, ]
        prec = sum(tl_subset$winningTeam == 100) / nrow(tl_subset)
        df_prec_rift[i + 1, j + 1] = prec
    }
}
rownames(df_prec_rift) = c('blue_team_0', 'blue_team_1')
colnames(df_prec_rift) = c('red_team_0', 'red_team_1')
df_prec_rift


# 蓝色方摧毁防御塔胜率
win_prec_tower = function(role, t1TowerTaken, t2TowerTaken) {
    df_prec_tower = data.frame() # 存储计算结果
    if (role == 'top' | role == 'bot') {
            towers = 4
            }
            else {
               towers = 6
            }
    for (i in 0:towers) {
        for (j in 0:towers) {
            tl_subset = timeline_840[t2TowerTaken == i & t1TowerTaken == j, ]
            prec = sum(tl_subset$winningTeam == 100) / nrow(tl_subset)
            df_prec_tower[i + 1, j + 1] = prec
        }
        rownames(df_prec_tower)[i + 1] = paste('blue_team_', i)
        colnames(df_prec_tower)[i + 1] = paste('red_team_', i)
    }
    return(df_prec_tower)
}
win_prec_tower('top', timeline_840$t1TopTowerTaken, timeline_840$t2TopTowerTaken)
win_prec_tower('mid', timeline_840$t1MidTowerTaken, timeline_840$t2MidTowerTaken)
win_prec_tower('bot', timeline_840$t1BotTowerTaken, timeline_840$t2BotTowerTaken)


# 各变量差值的相关系数矩阵
ggcorr(timeline_840_Diff[, c(28:32, 11:15)], method = c('pairwise', 'pearson'), label = TRUE, label_alpha = TRUE,
    label_round = 4, hjust = 0.5, name = '各变量差值的相关系数矩阵')


# 蓝色方获胜时各位置的对位经济差直方图
tl_gold_diff_blue = timeline_840_Diff[timeline_840_Diff$winningTeam == 100, c(11:15)] %>%
    gather(key = 'text', value = 'value') %>%
    mutate(text = gsub('\\.', ' ', text))

tl_gold_diff_blue_plot = tl_gold_diff_blue %>%
    mutate(text = fct_reorder(text, value)) %>%
    ggplot(aes(x = value, color = text, fill = text)) +
        geom_histogram(alpha = 0.5, binwidth = 5) +
        scale_fill_viridis(discrete = TRUE) +
        scale_color_viridis(discrete = TRUE) +
        theme(
        legend.position = 'none',
        panel.spacing = unit(0.1, 'lines'),
        strip.text.x = element_text(size = 8)
        ) +
        xlab('对位经济差') +
        ylab('蓝色方获胜时各位置的对位经济差直方图') +
        facet_wrap(~text)
tl_gold_diff_blue_plot


# 紫色方获胜时各位置的对位经济差直方图
tl_gold_diff_red = timeline_840_Diff[timeline_840_Diff$winningTeam == 200, c(11:15)] %>%
    gather(key = 'text', value = 'value') %>%
    mutate(text = gsub('\\.', ' ', text))

tl_gold_diff_red_plot = tl_gold_diff_blue %>%
    mutate(text = fct_reorder(text, value)) %>%
    ggplot(aes(x = value, color = text, fill = text)) +
        geom_histogram(alpha = 0.5, binwidth = 5) +
        scale_fill_viridis(discrete = TRUE) +
        scale_color_viridis(discrete = TRUE) +
        theme(
        legend.position = 'none',
        panel.spacing = unit(0.1, 'lines'),
        strip.text.x = element_text(size = 8)
        ) +
        xlab('对位经济差') +
        ylab('紫色方获胜时各位置的对位经济差直方图') +
        facet_wrap(~text)
tl_gold_diff_red_plot


# Shapiro-Wilk 检验
mvnormtest::mshapiro.test(t(as.matrix(timeline_840_Diff[timeline_840_Diff$winningTeam == 100, c(11:15)])))
mvnormtest::mshapiro.test(t(as.matrix(timeline_840_Diff[timeline_840_Diff$winningTeam == 200, c(11:15)])))


# BoxM 检验
MVTests::BoxM(timeline_840_Diff[, c(28, 29, 5:15)], timeline_840_Diff$winningTeam)
MVTests::BoxM(timeline_840_Diff[, c(28, 29, 11:15)], timeline_840_Diff$winningTeam)
MVTests::BoxM(timeline_840_Diff[, c(5:10)], timeline_840_Diff$winningTeam)


# 为了使结果可复现 从 random.org 获取一个随机数作为种子
set.seed(4225)

# 将数据集分为训练集和测试集，比例为 8:2
index_train = sample(c(TRUE, FALSE), nrow(timeline_840_Diff), replace = TRUE, prob = c(0.8, 0.2))
training = timeline_840_Diff[index_train, ]
testing = timeline_840_Diff[!index_train, ]

# 建立 Bayes 判别模型
lda_fit = MASS::lda(winningTeam ~ dragonDiff + riftDiff + t1TopTowerTaken + t2TopTowerTaken + t1MidTowerTaken +
    t2MidTowerTaken + t1BotTowerTaken + t2BotTowerTaken + topGoldDiff + jugGoldDiff + midGoldDiff +
    adcGoldDiff + supGoldDiff, prior = c(0.5, 0.5), data = training)

str(lda_fit)
lda_fit

# 训练集混淆矩阵
table(training$winningTeam, predict(lda_fit)$class)
# 训练集预测正确率
sum(training$winningTeam == predict(lda_fit)$class) / nrow(training) # 0.7578272

# 测试集混淆矩阵
table(testing$winningTeam, predict(lda_fit, newdata = testing)$class)
# 测试集预测正确率
sum(testing$winningTeam == predict(lda_fit, newdata = testing)$class) / nrow(testing)


# 找出训练集预测正确率最大的临界值
critical = function(dataframe) {
    prec_df = data.frame(critical = NA, prec = NA)
    j = 0
    for (i in seq(0.01, 0.99, 0.01)) {
        j = j + 1
        if (identical(dataframe, training)) {
            confus = table(dataframe$winningTeam, predict(lda_fit)$posterior[, '100'] >= i)
        }
        else{
            confus = table(dataframe$winningTeam, predict(lda_fit, newdata = dataframe)$posterior[, '100'] >= i)
        }
        prec = (confus[, 2][1] + confus[, 1][2]) / sum(confus)
        prec_df[j, ] = c(i, prec)
    }
    return(prec_df[which.max(prec_df$prec), ])
}
critical(training)
critical(testing)


# Bayes（Fisher）判别函数的直方图
plot(lda_fit, dimen = 1, type = 'b')

# Bayes 判别函数在测试集对位经济差上的分区图
klaR::partimat(winningTeam ~ topGoldDiff + jugGoldDiff + midGoldDiff + adcGoldDiff + supGoldDiff,
    data = testing, method = 'lda')


# 测试集正判样品
head(testing[testing$winningTeam == predict(lda_fit, newdata = testing)$class, ])
correct_testing_summary = summary(testing[testing$winningTeam == predict(lda_fit, newdata = testing)$class, ])
# 测试集预测正确时游戏时长的统计特征
# correct_testing_summary[,16]
summary(testing[testing$winningTeam == predict(lda_fit, newdata = testing)$class, 'gameLength'])


# 测试集误判样品
head(testing[!(testing$winningTeam == predict(lda_fit, newdata = testing)$class), ])
error_testing_summary = summary(testing[!(testing$winningTeam == predict(lda_fit, newdata = testing)$class), ])
# 测试集预测错误时游戏时长的统计特征
# error_testing_summary[,16]
summary(testing[!(testing$winningTeam == predict(lda_fit, newdata = testing)$class), 'gameLength'])


# 测试集预测正确和预测错误时游戏时长的小提琴图
correct_bayes_gl = testing[testing$winningTeam == predict(lda_fit, newdata = testing)$class, 'gameLength']
error_bayes_gl = testing[!(testing$winningTeam == predict(lda_fit, newdata = testing)$class), 'gameLength']
testing_bayes_gameLength = data.frame(
    bayes_gameLength = c(correct_bayes_gl, error_bayes_gl),
    results = c(rep('预测正确', length(correct_bayes_gl)), rep('预测错误', length(error_bayes_gl)))
)

ggplot(testing_bayes_gameLength, aes(x = results, y = bayes_gameLength, fill = results)) +
    geom_violin() +
    scale_fill_manual(labels = c('预测正确', '预测错误'), values = c('blue', 'red')) +
    labs(fill = '结果') +
    xlab('Bayes 判别预测结果') +
    ylab('游戏时长/秒') +
    ggtitle('测试集预测正确和预测错误时游戏时长的小提琴图')
