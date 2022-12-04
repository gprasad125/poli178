# pkgs to run
require("dplyr")
require("ggplot2")
require("plyr")
require("scales")
require("sf")
require("rnaturalearth")
require("rnaturalearthdata")

# values 
predictions_small = c(456.7929, 141.3165, 285.226,715.0438,970.1524,203.9044,482.2437, 250.0464, 501.769, 559.8317, 225.8891, 198.101, 662.7124, 524.652, 278.7491, 190.8226, 376.9305, 583.9873, 200.7259, 1072.5238, 330.3385, 464.6927, 152.7519, 168.1621, 239.8843, 3566.4944, 310.1258, 798.073, 3959.5351, 882.7092, 186.3606, 117.3206, 314.4628, 731.4753,278.9267, 362.0448, 195.3655, 633.5529, 481.3268, 304.5206, 1289.8236)
countries_small = c("Afghanistan", "Angola", "Azerbaijan","Bangladesh","Cambodia","Cameroon","Chad","Republic of Congo", "Democratic Republic of the Congo","Djibouti", "Ethiopia","Georgia", "Ghana","Guinea","Guinea Bissau","Guyana","India","Kenya","Kyrgyzstan","Laos","Liberia","Madagascar","Malawi","Maldives","Mongolia","Mozambique","Myanmar","Nepal", "Niger","Pakistan", "Rwanda","Samoa", "Republic of Serbia","Sri Lanka","Tajikistan","United Republic of Tanzania","Togo","Uganda", "Vietnam","Zambia","Zimbabwe")

predictions_large = c(804.1438,487.2559, 621.4066, 558.5931,630.2007, 604.6633, 503.1979, 562.7148, 620.4221, 623.9045, 674.7848, 806.9564, 876.7416, 932.8565,702.741, 404.6253, 441.3478, 854.4094, 859.7742, 642.707, 522.5877, 864.5221, 609.7837, 625.2519, 580.6382, 625.8042, 608.3198, 650.0199, 415.2145, 361.3925, 1060.3378, 650.6759, 367.6549, 548.6318, 819.7809, 655.9864, 570.3845, 757.588, 521.5229, 892.6532, 483.3945,586.2564, 847.7843, 868.1829, 528.3204, 589.0877, 687.5215, 592.5926, 485.788, 592.881, 599.1625, 532.5979, 865.8609, 510.7172, 810.5035, 575.8083, 512.3328, 785.3044, 339.5244, 886.7835, 535.3188, 920.5168, 484.4215, 875.3318, 765.1666, 607.8552, 988.4872, 394.0913, 385.1996, 857.9603, 653.8755, 646.1625, 485.7041, 952.9048, 504.3515, 753.4705, 890.7781, 834.8531, 662.3605, 550.0393, 641.4777, 621.519, 497.3926, 873.9706, 885.702, 332.6512, 301.8685, 875.8769, 604.1063, 628.4543, 465.2312, 483.536, 549.3607, 879.2242, 847.0095, 613.3336, 796.2236, 816.8717, 813.1978, 881.1364, 434.727, 388.0544,335.2832, 624.3479, 409.0301, 829.8127, 849.762,901.6617, 542.2802, 861.7373, 499.3813, 355.4869, 849.4195, 827.304,878.3866, 392.2497, 890.7498)
countries_large = c("Afghanistan","Angola", "Antigua and Barbuda","Argentina","Australia","Austria", "Azerbaijan","Bahamas","Bangladesh","Belarus","Belgium","Botswana","Brazil","Brunei","Bulgaria","Cameroon","Canada","Chad","Chile","Colombia","Republic of Congo","Croatia","Cuba","Czech Republic","Democratic Republic of the Congo","Denmark","Djibouti","Ecuador","Egypt","Ethiopia","Finland","France","Georgia","Germany","Ghana","Greece", "Guinea","Guinea Bissau","Guyana","Hungary","India","Indonesia","Iran","Iraq","Ireland","Israel","Italy","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Kuwait","Kyrgyzstan","Laos","Liberia","Luxembourg","Madagascar","Malawi","Malaysia","Maldives","Malta","Mauritius","Mexico","Mongolia","Morocco","Mozambique","Myanmar","Namibia","Nepal","Netherlands","New Zealand","Nicaragua","Niger","Nigeria","Norway","Oman","Pakistan","Panama","Papua New Guinea","Peru","Philippines","Poland","Portugal","Qatar","Rwanda","Samoa","Saudi Arabia","Republic of Serbia","Sierra Leone","Singapore","Slovenia","Solomon Islands","South Africa","Spain","Sri Lanka","Sudan","Sweden","Switzerland","Syria","Taiwan","Tajikistan","United Republic of Tanzania", "Thailand","Togo", "Turkey","Turkmenistan", "United Arab Emirates","Uganda", "Ukraine","United States of America", "Uzbekistan","Venezuela", "Vietnam","Yemen", "Zambia","Zimbabwe")

# convert to tibble 
tib = tibble(countries_small, predictions_small)
tib_large = tibble(countries_large, predictions_large)

names(tib) = c("Country", "Investment")
names(tib_large) = c("Country", "Investment")

# merge with world data
world = ne_countries(scale = "medium", returnclass = "sf")
results_small.world = merge(world, tib, by.x = "admin", by.y = "Country", all = TRUE)
results_large.world = merge(world, tib_large, by.x = "admin", by.y = "Country", all = TRUE)

# generate heatmap
heatmap_small = ggplot(results_small.world) + geom_sf(aes(fill = Investment)) + scale_fill_gradient(low = "#cadbfd", high = "#052a75", na.value = "red")
heatmap_large = ggplot(results_large.world) + geom_sf(aes(fill = Investment)) + scale_fill_gradient(low = "#cadbfd", high = "#052a75", na.value = "red")
