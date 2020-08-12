library(tidytuesdayR)
library(extrafont)
tuesdata <- tidytuesdayR::tt_load(2020, week = 33)
avatar <- tuesdata$avatar

#Filter instances where Zuko & Aang appear in same chapter
#Mutate to add variable which counts total number of scenes on each chapter
aako <- avatar %>% 
  filter(str_detect(full_text, "Zuko")) %>% 
  filter(str_detect(full_text, "Aang")) %>% 
  group_by(chapter) %>% 
  mutate(total_scenes = n()) %>% 
  mutate(xint = chapter_num)

#Highest number of Zuko/Aang chapter appearances per book
aako %>% 
  group_by(book_f, chapter, chapter_num, total_scenes) %>% 
  summarise() %>% 
  arrange(desc(total_scenes)) 
  
#Book names into factors > they will appear in correct order as facets
aako$book_f = factor(aako$book, levels = c("Water", "Earth", "Fire"))
#Character names into factors
aako$character <- as.factor(aako$character)

#Text labels for each facet
labz <- data_frame(
  label = c("The Siege of the North, Part 2\nWait is this actually The Winds of Winter?", "The Crossroads of Destiny\nA potential crossroads in the story?", "The Firebending Masters\nSPOILER ALERT!\nThis chapter seems important."), 
  book_num = c("1", "2", "3"),
  x = c(15.7, 15.7, 7.5),
  y = c(0.062, 0.071, 0.17),
  color = c("springgreen4", "firebrick", "dodgerblue"),
  fill = c("gold", "black", "darkblue")
)

#Book names for labeller---this will change numbers to names in facet
booknames <- c(
  "1" = "Water",
  "2" = "Earth",
  "3" = "Fire"
)

#Let's plot density curves
p1 <- ggplot(aako) +
  geom_density(aes(x = chapter_num, fill = book, color = book), alpha = 0.75) +
  scale_fill_manual(values = c("gold", "firebrick", "springgreen4", "firebrick", "dodgerblue", "dodgerblue"), name = "Book") +
  scale_color_manual(values = c("black", "gold", "black", "springgreen4", "darkblue", "darkblue")) +
  xlab("Chapter Number") +
  ylab("Density") +
  facet_grid(~book_num, labeller = labeller(book_num = booknames)) +
  theme(
    text = element_text(family = "Tempus Sans ITC", face = "bold"),
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "papayawhip"),
    plot.background = element_rect(fill = "papayawhip"),
    axis.title.y = element_text(size = 18, face = "italic"),
    axis.text.y = element_text(size = 14),
    plot.margin = margin(t = 1, r = 1, b = 0, l = 1, unit = "pt"),
    plot.title = element_text(size = 18),
    strip.text = element_text(color = "black", size = 14),
    strip.background = element_rect(fill = "papayawhip", color = "black"),
  ) +
  geom_label(data = labz, mapping = aes(x = x, y = y, label = label, color = color, fill = fill), alpha = 0.75, size = 2.5) +
  geom_segment(aako %>% 
               filter(book_num == "1"), mapping = aes(x = 20, xend = 20, y = 0, yend = 0.04), color = "darkblue", lty = "dotdash", size = 1) +
  geom_segment(aako %>% 
               filter(book_num == "2"), mapping = aes(x = 20, xend = 20, y = 0, yend = 0.051), color = "gold", lty = "dotdash", size = 1) +
  geom_segment(aako %>% 
               filter(book_num == "3"), mapping = aes(x = 13, xend = 13, y = 0, yend = 0.191), color = "black", lty = "dotdash", size = 1) +
  labs(
    title = "Everybody's Talkin' 'Bout...Aang & Zuko",
    subtitle = "When are both Aang & Zuko mentioned in the same scene, and who mentions them?")

#Plot with geom_count (reorder $character to display from A to Z )
p2 <- ggplot(aako, mapping = aes(x = chapter_num, y = reorder(character, desc(character)))) +
         geom_count(aes(fill = book_f, color = book_f, size = factor(total_scenes)), shape = 21, alpha = 0.75) +
  scale_fill_manual(values = c("dodgerblue", "springgreen4", "firebrick"), name = "Book") +
  scale_color_manual(values = c("darkblue", "gold", "black")) +
  scale_size_manual(values = c(2, 2.5, 3, 4, 4.5, 5, 6, 6.5, 7.5, 8, 8.5, 9.5)) +
  theme(
    text = element_text(family = "Tempus Sans ITC", face = "bold"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    plot.margin = margin(t = 0, r = 1, b = 0, l = 1, unit = "pt"),
    panel.background = element_rect(fill = "papayawhip"),
    plot.background = element_rect(fill = "papayawhip"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  ) +
  facet_wrap(~book_num, dir = "h") +
  xlab("Chapter Number") +
  ylab("Character Name") +
  labs(caption = "TidyTuesday Week 33\nThe data this week comes from the [`appa` R package](https://github.com/averyrobbins1/appa) created by [Avery Robbins](https://twitter.com/robbins_ave)\nVisualization by Daniel Pearson (Twitter: @itsdanpearson)")

#Patchwork <3
avpatch = p1 / p2 

  