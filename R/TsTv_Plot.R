# vivaldi package
# Kate Johnson

TsTv_Plot = function(df, wkdir, freq_cutoff = 0.02, coverage_cutoff = 300){

    if (!dir.exists(glue("{wkdir}/SNV_figures"))) {
        dir.create(glue("{wkdir}/SNV_figures"))
      }

    xlim = length(levels(factor(df$sample)))

    p1 = ggplot(df, aes(x=sample,y=R)) +
        geom_point() +
        theme_bw() +
        ggtitle("K2P") +
        ylab("Transition/Transversion ratio") +
        theme(legend.key = element_blank(),
            strip.background = element_rect(colour="black", fill="white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    print(p1)

    ggsave(p1,
       filename = glue("{wkdir}SNV_figures/TsTv.K2P.{freq_cutoff}.{coverage_cutoff}.pdf"),
       width = 0.3*xlim,
       height = 6, limitsize=FALSE,
          useDingbats = FALSE)

    p2 = ggplot(df, aes(x=sample,y=basic_tstv)) +
        geom_point() +
        theme_bw() +
        ggtitle("Basic Ts/Tv") +
        ylab("Transition/Transversion ratio") +
        theme(legend.key = element_blank(),
            strip.background = element_rect(colour="black", fill="white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    print(p2)

    ggsave(p2,
       filename = glue("{wkdir}SNV_figures/TsTv.basic.{freq_cutoff}.{coverage_cutoff}.pdf"),
       width = 0.3*xlim,
       height = 6, limitsize=FALSE,
          useDingbats = FALSE)


}
