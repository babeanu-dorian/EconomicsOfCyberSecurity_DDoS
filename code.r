# Load data:

colClasses <- c(NA, "Date", NA, NA, "NULL", "NULL", NA, NA, NA, "NULL", NA, "NULL", "NULL", "NULL", "NULL", NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", NA)
data <- read.csv(file = "data.csv", sep = "|", colClasses = colClasses)

# Setup data for NL:

data_nl <- data[data$raw_country == 'Netherlands',]
attacks_nl_host <- droplevels(aggregate(data_nl$packets, by=list(host=data_nl$raw_hostname), FUN=length))

nl_orgs <- c('onsbrabantnet', 'gogrid', 'choopa', 'telfort', 'vultr', 'upc', 'softlayer', 'ziggo', 'ddfr', 'beastnode', 'simrai', 'leasevps', 'worldstream', 'blazingfast', 'i3d', 'leaseweb', 'orangelemon', 'stackip', 'versatel', 'kpn')

# Total attacks per organization NL:

attacks_nl_org_matrix = matrix(ncol = 2, nrow=length(nl_orgs))

for(org_idx in 1:length(nl_orgs)){
	attacks_nl_org_matrix[org_idx, 1] <- nl_orgs[org_idx]
	attacks_nl_org_matrix[org_idx, 2] <- sum(attacks_nl_host[grepl(nl_orgs[org_idx], attacks_nl_host$host, ignore.case=T),]$x)
}
attacks_nl_org <- data.frame(attacks_nl_org_matrix)
colnames(attacks_nl_org) <- c('organization', 'count_attacks')

attacks_nl_org$organization <- as.character(attacks_nl_org$organization)
attacks_nl_org$count_attacks <- as.numeric(as.character(attacks_nl_org$count_attacks))

par(bg="white")
par(mai=c(1,2,1,1))
barplot(attacks_nl_org$count_attacks, main='Attacks per organization (Netherlands)', names=attacks_nl_org$organization, horiz=T, las=1, xlab = '# attacks', xlim = range(pretty(c(0, attacks_nl_org$count_attacks))))

# Total packets per organization NL:

packets_nl_host <- droplevels(aggregate(data_nl$packets, by=list(host=data_nl$raw_hostname), FUN=sum))
packets_nl_org_matrix = matrix(ncol = 2, nrow=length(nl_orgs))

for(org_idx in 1:length(nl_orgs)){
	packets_nl_org_matrix[org_idx, 1] <- nl_orgs[org_idx]
	packets_nl_org_matrix[org_idx, 2] <- sum(packets_nl_host[grepl(nl_orgs[org_idx], packets_nl_host$host, ignore.case=T),]$x)
}

packets_nl_org <- data.frame(packets_nl_org_matrix)
colnames(packets_nl_org) <- c('organization', 'packets')

packets_nl_org$organization <- as.character(packets_nl_org$organization)
packets_nl_org$packets <- as.numeric(as.character(packets_nl_org$packets))

barplot(packets_nl_org$packets, main='Total attack packets per organization (Netherlands)', names=packets_nl_org$organization, horiz=T, las=1, xlab = '# packets', xlim = range(pretty(c(0, packets_nl_org$packets))))

# Total attack duration per organization NL:

duration_nl_host <- droplevels(aggregate(data_nl$duration, by=list(host=data_nl$raw_hostname), FUN=sum))
duration_nl_org_matrix = matrix(ncol = 2, nrow=length(nl_orgs))

for(org_idx in 1:length(nl_orgs)){
	duration_nl_org_matrix[org_idx, 1] <- nl_orgs[org_idx]
	duration_nl_org_matrix[org_idx, 2] <- sum(duration_nl_host[grepl(nl_orgs[org_idx], duration_nl_host$host, ignore.case=T),]$x)
}

duration_nl_org <- data.frame(duration_nl_org_matrix)
colnames(duration_nl_org) <- c('organization', 'duration')

duration_nl_org$organization <- as.character(duration_nl_org$organization)
duration_nl_org$duration <- as.numeric(as.character(duration_nl_org$duration))

barplot(duration_nl_org$duration, main='Total attack duration per organization (Netherlands)', names=duration_nl_org$organization, horiz=T, las=1, xlab = 'duration (s)', xlim = range(pretty(c(0, duration_nl_org$duration))))

# Attacks per service NL:

attacks_nl_service <- droplevels(aggregate(data_nl$packets, by=list(service=data_nl$service), FUN=length))
barplot(attacks_nl_service$x, main = "Attacks per service (Netherlands)", names=attacks_nl_service$service, ylab = '# attacks', ylim=range(pretty(c(0, attacks_nl_service$x))))

# Attacks on Ziggo in 2014

attacks_host_service_date <- droplevels(aggregate(data_nl$packets, by=list(host=data_nl$raw_hostname, service=data_nl$service, date=data_nl$date), FUN=length))
attacks_ziggo_dns <- droplevels(attacks_host_service_date[grepl('ziggo', attacks_host_service_date$host, ignore.case=T) & attacks_host_service_date$service == 'dns' & attacks_host_service_date$date < as.Date('2015-01-01'),])
attacks_ziggo_dns_day <- droplevels(aggregate(attacks_ziggo_dns$x, by=list(date=attacks_ziggo_dns$date), FUN=sum))

attacks_ziggo_dns_month_matrix = matrix(ncol = 2, nrow=12)
months = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
for(month in 1:12){
	attacks_ziggo_dns_month_matrix[month, 1] <- months[month]
	attacks_ziggo_dns_month_matrix[month, 2] <- sum(attacks_ziggo_dns_day[month == as.numeric(format(attacks_ziggo_dns_day$date, '%m')),]$x)
}
attacks_ziggo_dns_month <- data.frame(attacks_ziggo_dns_month_matrix)
colnames(attacks_ziggo_dns_month) <- c('Month', 'attacks')
attacks_ziggo_dns_month$Month <- as.character(attacks_ziggo_dns_month$Month)
attacks_ziggo_dns_month$attacks <- as.numeric(as.character(attacks_ziggo_dns_month$attacks))
barplot(attacks_ziggo_dns_month$attacks, main = "DDoS attacks targeting Ziggo dns servers in 2014", names=attacks_ziggo_dns_month$Month, ylab = '# attacks', ylim=range(pretty(c(0, attacks_ziggo_dns_month$attacks))))