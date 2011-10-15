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
require(XML)

instances.from.reservation <- function(reservation.id,verbose=FALSE) {
    ec2din(filters=paste("reservation-id",reservation.id,sep="="),verbose=verbose)[[reservation.id]]
}

pending.instance <- function(reservation.id) {
    instances <- instances.from.reservation(reservation.id)

    ## test both state and public dns status
    if(any(is.na(instances[,"instanceState"]) | is.na(instances[,"dnsName"]))) {
        ans <- TRUE
    } else {
        ans <- any(instances[,"instanceState"]=="pending")
    }
    ans
}

sleep.while.pending <- function(reservation.id,sleep.time=2,verbose=TRUE) {
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
    sleep.while.pending(reservation[1],verbose)
    instances <- instances.from.reservation(reservation[1])
    ans <- list(reservation=reservation,instances=instances)
    class(ans) <- "ec2.cluster"
    ans
}

instance.xml.to.dataframe <- function(reservation.id,owner.id,x) {
    minimal.colnames <- c("reservationId","ownerId","instanceId","imageId","instanceState","privateDnsName","dnsName","reason","keyName","amiLaunchIndex","productCodes","instanceType","launchTime","placement","kernelId","monitoring","privateIpAddress","ipAddress","groupSet","architecture","rootDeviceType","rootDeviceName","virtualizationType","clientToken","hypervisor")
    ans <- vector("list",length(minimal.colnames))
    for(i in 1:length(ans)) { ans[[i]] <- NA }
    names(ans) <- minimal.colnames

    ans[["reservationId"]] <- reservation.id
    ans[["ownerId"]] <- owner.id

    all.nodes <- names(x)
    nested.nodes <- c("instanceState","blockDeviceMapping","tagSet")

    ## make sure these nodes exist in this aws results set
    nested.nodes <- nested.nodes[nested.nodes %in% all.nodes]

    simple.nodes <- all.nodes[-match(nested.nodes,all.nodes)]
    for(nm in simple.nodes) {
        if(nm %in% minimal.colnames) {
            ans[[ nm ]] <- ifelse(is.null(xmlValue(x$children[[nm]])),NA,xmlValue(x$children[[nm]]))
        }
    }

    ## why is instanceState a complex node...?
    ans[["instanceState"]] <- xmlValue(x$children$instanceState$children$name)

    as.data.frame(ans,stringsAsFactors=FALSE)
}

reservation.xml.to.dataframe <- function(x) {
    reservation.id <- xmlValue(x$children$reservationId)
    owner.id <- xmlValue(x$children$ownerId)
    ans <- list()
    for(instance in x$children$instancesSet$children) {
        ans[[ xmlValue(instance$children$instanceId) ]] <- instance.xml.to.dataframe(reservation.id,owner.id,instance)
    }
    ##group.set <- xmlValue(x$children$groupSet)
    do.call(rbind,ans)
}

get.instances.from.cluster <- function(cluster) {
    cluster[["instances"]][,"instanceId"]
}

ec2din.format.xml <- function(x) {
    ans <- list()
    x.root <- xmlRoot(xmlTreeParse(paste(x,collapse=""),asText=TRUE))
    for(reservation in x.root$children$reservationSet$children) {
        ans[[ xmlValue(reservation$children$reservationId) ]] <- reservation.xml.to.dataframe(reservation)
    }
    ans
}

ec2din <- function(instance=NULL,filters=NULL,verbose=FALSE) {
    aws.cmd <- paste("ec2-describe-instances",
                     ifelse(instance,instance,""),
                     "--verbose",
                     ifelse(!is.null(filters),paste("--filter",filters,collapse=" "),""),
                     "|sed -n '/DescribeInstancesResponse/,/\\/DescribeInstancesResponse/p'")
    if(verbose) {
        cat("ec2din, using this cmd:\n")
        print(aws.cmd)
    }
    ec2din.format.xml(system(aws.cmd,intern=TRUE))
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
    ec2stop.instances(instances[,"instanceId"])
}

ec2terminate.reservation <- function(reservation.id) {
    instances <- instances.from.reservation(reservation.id)
    ec2terminate.instances(instances[,"instanceId"])
}
