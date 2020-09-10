plotModuleEigenproteinsBySample <- function(MEPs, module, filename){
  selectedEigenproteinModule <- MEPs%>%
    filter(`Module` == module)
  ggplot(data = selectedEigenproteinModule, 
         mapping = aes(x = `ModuleEigenprotein`, 
                       y = `Experiment`))+
    geom_col()+
    ggtitle(paste(module))+
    theme_bw(base_family = "Helvetica")
  ggsave(filename = filename, plot = last_plot(), device = "pdf")
  message("Module eigenproteins exported to Results folder")
}