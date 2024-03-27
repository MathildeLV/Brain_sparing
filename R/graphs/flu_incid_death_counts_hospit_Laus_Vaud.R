# Plot parameter
lwd_size <- 1.2
text_size <- 15
legend_size <- 16
axis_legend_size <- 15
title_size <- 15

col_pal <- ggsci::pal_jco()(8)

lims1 <- as.POSIXct(ymd("1916-01-01"))    
lims2 <- as.POSIXct(ymd("1920-12-18"))    
lims3 <- as.POSIXct(ymd("1918-09-15"))    
lims4 <- as.POSIXct(ymd("1919-09-30"))    

datlim1 <- as.POSIXct(ymd("1918-07-01"))
datlim2 <- as.POSIXct(ymd("1919-03-31"))
datlim3 <- as.POSIXct(ymd("1920-01-01"))
datlim4 <- as.POSIXct(ymd("1920-03-31"))


dataZH_b     <- laus_vd_waves %>%
  mutate(Tag = substr(Wochenende,1,2),
      Monat = substr(Wochenende, 4,5),
      # Jahr = paste0(19,substr(Wochenende, 9,10)),
      Woche = ymd(paste0(Year,"-",Monat, "-", Tag)),
      KW = isoweek(Woche))%>%
  mutate(Namex= paste0(Year,"/W",KW),
         DeathsInc = Deaths/PopCity*1000,
         InfluenzaInc= Influenza_Stadt/PopCity*1000,
         HospInc = Total_Aufnahmen/PopCity*1000,
         AndereInc = Andere_Infekt/PopCity*1000,
         InfluenzaCantonInc = Influenza_Kanton/PopCanton*1000,
         HospInf = Total_Aufnahmen - Andere_Infekt,
         HospInfInc =  HospInf/PopCity*1000 ) 



FigureInc <- ggplot() +
  geom_line(data=dataZH_b,aes(y=InfluenzaCantonInc,x=as.POSIXct(Woche),colour="Canton Vaud"), 
            show.legend = FALSE, lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims1), max(lims2)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                     values = c(col_pal[4],  col_pal[1]))+
  xlab("Month/Year")+
  ylab("per 1,000 inhab.")+
  ggtitle("A) Newly reported ILI cases in the Canton of Vaud") +
  geom_vline(xintercept = lims3, linetype="dashed")+
  geom_vline(xintercept = lims4, linetype="solid")+
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  theme_bw()+
  theme(axis.text.y = element_text(size=text_size),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = c(.08, .8),
    legend.text=element_text(size=legend_size),
    axis.text.x = element_blank(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(size=axis_legend_size),
    title =element_text(size=title_size))
FigureInc


FigureDeath <- ggplot() +
  geom_line(data=dataZH_b ,aes(y=DeathsInc,x= as.POSIXct(Woche),col="Deaths"), 
             show.legend = FALSE, lwd=lwd_size )+
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims1), max(lims2)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                   values =col_pal[7])+
  xlab("Month/Year")+
  ylab("per 1,000 inhab.")+
  ggtitle("B) Deaths (all-cause) City of Lausanne") +
  geom_vline(xintercept = lims4, linetype="solid")+
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  theme_bw()+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))
FigureDeath

FigureHospital <- ggplot() +
  geom_line(data=dataZH_b,aes(y=AndereInc,x=as.POSIXct(Woche),
               colour="Subgroup infections (incl. influenza)"),
               show.legend = FALSE, lwd=lwd_size ) +
  # geom_line(data=dataZH_b,aes(y= HospInc,x=as.POSIXct(Woche),colour="Total"), lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims1), max(lims2)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                     values = c(col_pal[2],col_pal[8]))+
  xlab("Month/Year")+
  ylab("per 1,000 inhab.")+
  ggtitle("C) Hospitalisations in the Canton Vaud, due to infections (incl. influenza)") +
  geom_vline(xintercept = lims4, linetype="solid")+
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  theme_bw()+
  theme(axis.text.y = element_text(size=text_size),
        axis.text.x = element_text(size=16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(size=axis_legend_size), 
    title =element_text(size=title_size))
FigureHospital

plot_lausanne <- cowplot::plot_grid(FigureInc,FigureDeath,FigureHospital, 
                                  ncol=1, nrow=3, align="hv",
                                  rel_heights = c(1,1,1))

plot_lausanne
cowplot::save_plot("Vaud_ILI_Deaths_Lausanne_hospit_Vaud.pdf", plot_lausanne , path=here("output/graphs/suppl_data"), base_height=15,base_width=20)
