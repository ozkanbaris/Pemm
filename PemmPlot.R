library(gridExtra)
library(grid)
library(ggplotify)
library(ggplot2)
library(ggalluvial)
source("LoadPemm.R")
eNames<-c("Design","Performer","Owner","Infrastructure","Metrics")
cNames<-c( "Purpose", "Context", "Documantation","Knowledge", "Skills", "Behavior", "Identity", "Activities", 
           "Authority","IS", "HRS","Definition", "Uses")
assdata <-
  inner_join(
    melted_post,
    melted_pre,
    by = c(
      "enabler" = "enabler",
      "plev" = "plev",
      "comp" = "comp",
      "Persoon" = "Persoon"
    )
  ) %>%
  select(-variable.x,-variable.y) %>% mutate(vdiff = postv - prev)
allflows <- assdata  %>% group_by(enabler, comp, plev, prev, postv) %>% summarise(freq = n())




# pointPlot at P levels
pointPlotP<-ggplot( allflows, aes(prev, postv, size=freq )) +
  geom_point(alpha=0.4, shape=21, stroke = 1.5)+
  scale_size_continuous(range = c(2,5), trans="identity")+
  geom_abline(intercept = 0, slope = 1, colour="#E41A1C") +
  theme_bw() +
  # scale_colour_manual("my3cols")+
  theme(axis.title=element_blank(), axis.text = element_blank() , legend.position="none") +
  coord_cartesian(xlim = 0:3,ylim =0:3)+
  facet_wrap( vars(enabler,comp,plev), ncol=4,labeller = label_wrap_gen(multi_line=FALSE))+
  theme(
    strip.background = element_blank(),
    # strip.text.x = element_blank()
  )



#Aggregation pointPLot at C levels with color P
set.seed(955)
# allflowsCP <- allflows %>%  filter(!(prev ==0 | postv ==0)  )

pointPlotPC<-ggplot( allflows, aes(prev, postv, size=freq, color=plev )) +
  geom_point(alpha=0.4, shape=21, position=position_jitter(h=0.05,w=0.05), stroke = 1.5)+
  scale_size_continuous(range = c(1, 12), trans="identity")+
  geom_abline(intercept = 0, slope = 1, colour="#E41A1C") +
  theme_bw() +
  # scale_colour_manual("my3cols")+
  # theme(axis.title=element_blank(), axis.text = element_blank() , legend.position="none") +
  coord_cartesian(xlim = 1:3,ylim =1:3)+
  facet_wrap( vars(enabler,comp), ncol=1,labeller = label_wrap_gen(multi_line=FALSE))+theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    
  )



#Aggregation pointPLot at C levels
set.seed(955)
allflowsC<- allflows %>% group_by(enabler, comp, prev, postv) %>% summarise(bfreq=sum(freq))

pointPlotC<-ggplot( allflowsC, aes(prev, postv, size=bfreq )) +
  geom_point(alpha=0.4, shape=21, stroke = 1.5)+
  scale_size_continuous(range = c(1, 12), trans="identity")+
  geom_abline(intercept = 0, slope = 1, colour="#E41A1C") +
  theme_bw() +
  # scale_colour_manual("my3cols")+
  # theme(axis.title=element_blank(), axis.text = element_blank() , legend.position="none") +
  coord_cartesian(xlim = 1:3,ylim =1:3)+
  facet_wrap( vars(enabler,comp), ncol=1,labeller = label_wrap_gen(multi_line=FALSE))+theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )



# grid.arrange(pointPlotC,pointPlotPC,pointPlotP,widths = unit( c(1,1,4), c("null","null","null","null")),ncol=3)


#Aggregation alluvial at P levels
dfC <- allflows %>%  filter(!(prev ==0 | postv ==0)  )
dfC$prev=factor(dfC$prev, levels = c(3, 2, 1))
dfC$postv=factor(dfC$postv, levels = c(3, 2, 1))

allPAlluv<-ggplot(dfC, aes(y = freq, axis1 = prev, axis2 = postv)) +
  geom_flow(aes(fill=prev), width = 1/14, alpha=0.5) +
  geom_stratum(width = 1/14)+
  geom_text(stat = "stratum", label.strata = TRUE,  size = 1) +
  scale_x_continuous(breaks = 1:2, labels = c("Pre", "Post")) +
  # scale_fill_manual(values  = c(A_col, B_col, C_col)) +
  # scale_color_manual(values = c(A_col, B_col, C_col)) +
  theme_grey() +
  theme(axis.title=element_blank(), axis.text = element_blank() , legend.position="none") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size= 5),
    axis.ticks  = element_blank(),
    axis.text.x = element_text(size = 5)
  )+
  facet_wrap(  ~enabler+comp+plev, ncol=4,labeller = label_wrap_gen(multi_line=FALSE),scales="free_y")+
  theme(
    # strip.background = element_blank(),
    # strip.text.x = element_blank()
  )

#Aggregation alluvial at C levels with P colored
allCAlluv<-ggplot(dfC, aes(y = freq, axis1 = prev, axis2 = postv)) +
  geom_flow(aes(fill=plev), width = 1/14, alpha=0.5) +
  geom_stratum(width = 1/14, alpha=0.5)+
  geom_text(stat = "stratum", label.strata = TRUE,  size = 1) +
  scale_x_continuous(breaks = 1:2, labels = c("Pre", "Post")) +
  # scale_fill_manual(values  = c(A_col, B_col, C_col)) +
  # scale_color_manual(values = c(A_col, B_col, C_col)) +
  theme_minimal() +
  theme(axis.title=element_blank(), axis.text = element_blank() , legend.position="none") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks  = element_blank(),
    axis.text.x = element_text(size = 5))+
    facet_wrap(  ~enabler+comp, ncol=1,labeller = label_wrap_gen(multi_line=FALSE),scales="free_y")+theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

# # header
# grobs<-list(
#   grobTree(rectGrob(gp=gpar(fill=2, alpha=0.5,fontsize = 5)), textGrob("En")),
#   grobTree(rectGrob(gp=gpar(fill=2, alpha=0.5,fontsize = 5)), textGrob("comp")),
#   grobTree(rectGrob(gp=gpar(fill=2, alpha=0.5,fontsize = 5)), textGrob("Comphere")),
#   grobTree(rectGrob(gp=gpar(fill=4, alpha=0.5)), textGrob(levs[1])),
#   grobTree(rectGrob(gp=gpar(fill=5, alpha=0.5)),textGrob(levs[2])),
#   grobTree(rectGrob(gp=gpar(fill=6, alpha=0.5)), textGrob(levs[3])),
#   grobTree(rectGrob(gp=gpar(fill=7, alpha=0.5)), textGrob(levs[4])))
# 
# 
# grobs<-c(grobs,list(allCAlluv,pointPlotP, textGrob("CNAME")))
# 
# 
# lay <- rbind(
#   c(1,2,3,4,5,6,7),
#   c(1,10,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9),
#   c(1,NA,8,9,9,9,9)
#   
# )
# 
# grid.arrange(grobs = grobs,
#              layout_matrix = lay,
#              widths = unit(c(1, 1, 10, 10,10, 10,10),
#                c("null", "null", "null", "null", "null", "null", "null")
#              ))



#One Comp Test
# dfC <- allflows %>%  filter(enabler == "D", comp == "P")%>% ungroup() %>% select(plev,prev,postv,freq) %>%  filter(!(prev ==0 | postv ==0)  )
# dfC$prev=factor(dfC$prev, levels = c(3, 2, 1))
# dfC$postv=factor(dfC$postv, levels = c(3, 2, 1))
# # levels(dfC$postv) <- rev(levels(dfC$postv))
# # levels(dfC$prev) <- rev(levels(dfC$prev))
# set.seed(564)
# ggplot(dfC, aes(y = freq, axis1 = prev, axis2 = postv)) +
#   geom_alluvium(aes(fill=plev), width = 1/12, alpha=0.3) +
#   geom_stratum(width = 1/12)+
#   geom_text(stat = "stratum", label.strata = TRUE) +
#   scale_x_continuous(breaks = 1:2, labels = c("Pre", "Post")) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     panel.grid.major = element_blank(), 
#     panel.grid.minor = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks  = element_blank(),
#     axis.text.x = element_text(size = 2)
#   )