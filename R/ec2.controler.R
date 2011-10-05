###########################################################################
## Copyright (C) 2011  Whit Armstrong                                    ##
##                                                                       ##
## This program is free software: you can redistribute it and#or modify  ##
## it under the terms of the GNU General Public License as published by  ##
## the Free Software Foundation, either version 3 of the License, or     ##
## (at your option) any later version.                                   ##
##                                                                       ##
## This program is distributed in the hope that it will be useful,       ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of        ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ##
## GNU General Public License for more details.                          ##
##                                                                       ##
## You should have received a copy of the GNU General Public License     ##
## along with this program.  If not, see <http:##www.gnu.org#licenses#>. ##
###########################################################################

instances.from.reservation <- function(reservation.id,verbose=FALSE) {
    res <- ec2din(filters=paste("reservation-id",reservation.id,sep="="),verbose=verbose)
    instances <- res[grep("^INSTANCE",res)]
    instances.to.dataframe(instances)
}

pending.instance <- function(reservation.id) {
    instances <- instances.from.reservation(reservation.id)

    ## test both state and public dns status
    any(instances[,"InstanceState"]=="pending" || instances[,"PublicDNS"] == "(nil)")
}

sleep.while.pending <- function(reservation.id,sleep.time=1,verbose=FALSE) {
    while(pending.instance(reservation.id)) {
        if(verbose) { cat(".") }
        Sys.sleep(sleep.time)
    }
    if(verbose) { cat("\n") }
}

startCluster <- function(ami,key,instance.count,instance.type,verbose=FALSE) {
    cmd <- paste("ec2-run-instances",
                 ami,
                 "--show-empty-fields",
                 "--key",key,
                 "--instance-count",instance.count,
                 "--instance-type",instance.type)

    if(verbose) {
        cat("using this cmd:\n")
        print(cmd)
    }
    res <- system(cmd,intern=TRUE)
    reservation <- strsplit(res[[1]],split="\t")[[1]][-1]
    sleep.while.pending(reservation[1])
    instances <- instances.from.reservation(reservation[1])
    ans <- list(reservation=reservation,instances=instances)
    class(ans) <- "ec2.cluster"
    ans
}

get.master <- function(cluster) {
    if(class(cluster) != "ec2.cluster") {
        stop("need class of type: ec2.cluster.")
    }
    instances <- cluster[["instances"]]
    ans <- as.list(instances[which.min(as.integer(instances[,"AMILaunchIndex"])),,drop=F])
    names(ans) <- colnames(instances)
    ans
}

instances.to.dataframe <- function(x) {
    instances <- do.call(rbind,strsplit(x,split="\t"))
    colnames(instances) <- c("TypeIdentifier","InstanceID","AmiID","PublicDNS","PrivateDNS","InstanceState","KeyName","AMILaunchIndex","ProductCodes","InstanceType","InstanceLaunchTime","AvailabilityZone","KernelID","RAMDiskID","MonitoringState","PublicIPAddress","PrivateIPAddress","Tenancy","SubnetID","VpcID","TypeOfRootDevice","PlacementGroup","VirtualizationType","IDsOfEachSecurityGroups","Tags","HypervisorType","BlockdeviceIdentifier")
    instances
}

get.instances.from.cluster <- function(cluster) {
    cluster[["instances"]][,"InstanceID"]
}

ec2din <- function(instance=NULL,filters=NULL,verbose=FALSE) {
    aws.cmd <- paste("ec2-describe-instances",
                     ifelse(instance,instance,""),
                     "--show-empty-fields",
                     ifelse(!is.null(filters),paste("--filter",filters,collapse=" "),""))
    if(verbose) {
        cat("ec2din, using this cmd:\n")
        print(aws.cmd)
    }

    system(aws.cmd,intern=TRUE)
}

ec2stop.instances <- function(instance.ids) {
    cmd <- paste("ec2-stop-instances",paste(instance.ids,collapse=" "))
    res <- system(cmd,intern=TRUE)
    do.call(rbind,strsplit(res,"\t"))
}

ec2terminate.instances <- function(instance.ids) {
    cmd <- paste("ec2-terminate-instances",paste(instance.ids,collapse=" "))
    res <- system(cmd,intern=TRUE)
    do.call(rbind,strsplit(res,"\t"))
}

stopCluster <- function(cluster) {
    ec2stop.instances(get.instances.from.cluster(cluster))
}

terminateCluster <- function(cluster) {
    ec2terminate.instances(get.instances.from.cluster(cluster))
}

ec2stop.reservation <- function(reservation.id) {
    instances <- instances.from.reservation(reservation.id)
    ec2stop.instances(instances[,"InstanceID"])
}

ec2terminate.reservation <- function(reservation.id) {
    instances <- instances.from.reservation(reservation.id)
    ec2terminate.instances(instances[,"InstanceID"])
}
