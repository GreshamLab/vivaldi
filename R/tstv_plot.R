# vivaldi package
# Kate Johnson

tstv_plot = function(df){

    p1 = ggplot(df, aes(x=sample,y=R)) +
        geom_point() +
        theme_bw() +
        ggtitle("K2P") +
        ylab("Transition/Transversion ratio") +
        theme(legend.key = element_blank(),
            strip.background = element_rect(colour="black", fill="white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    print(p1)

    p2 = ggplot(df, aes(x=sample,y=basic_tstv)) +
        geom_point() +
        theme_bw() +
        ggtitle("Basic Ts/Tv") +
        ylab("Transition/Transversion ratio") +
        theme(legend.key = element_blank(),
            strip.background = element_rect(colour="black", fill="white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    print(p2)

}
