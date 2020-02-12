install.packages("Lahman")
install.packages("ggplot2")
require("dplyr")
library(Lahman)
library(ggplot2)
data(Batting)

Batting <- battingStats(
  data = Lahman::Batting,
  idvars = c("playerID", "yearID", "stint", "teamID", "lgID"),
  cbind = TRUE
)


eligibleHitters <- Batting %>%
  filter(yearID >= 1900 & AB > 450)

bestHitters <- eligibleHitters %>%
  group_by(yearID) %>%
  filter(BA == max(BA) | BA >= .400) %>%
  mutate(ba400 = BA >= 0.400) %>%
  select(playerID, yearID, BA, ba400)

sum(bestHitters$ba400)
mean(bestHitters$BA)



#how likely is it that a player wins the batting title in a season if the reach the mean mark?

n_samples <- 10000
bootstrap_ball <-
  replicate(n_samples, mean(sample(bestHitters$BA, replace = TRUE)))

ci <-
  quantile(bootstrap_ball,
           probs = c(0.025, 0.975),
           CI.level = .95)
ci

mean(bootstrap_ball)
plot(density(bootstrap_ball))
abline(v = ci, col = "blue")

ci <- as.data.frame(ci)
lower <- ci[1, ]
upper <- ci[2, ]
lower <- as.data.frame(lower)
upper <- as.data.frame(upper)

bs_ball <- as.data.frame(bootstrap_ball)
ggplot(bs_ball, aes(x = bootstrap_ball)) +
  geom_density() +
  geom_vline(aes(xintercept = lower)) + geom_vline(aes(xintercept = upper))


ba <- ggplot(bs_ball, aes(x = bootstrap_ball)) +
  geom_density(color = "darkblue", fill = "lightblue") + labs(title = "Top Hitters Batting Average density curve", x =
                                                                "Player Batting Average", y = "Density")


ba + geom_vline(
  aes(xintercept = mean(bootstrap_ball)),
  color = "blue",
  linetype = "dashed",
  size = 1
)
