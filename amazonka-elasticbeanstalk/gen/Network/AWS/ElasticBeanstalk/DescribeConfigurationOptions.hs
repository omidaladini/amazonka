{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the configuration options that are used in a particular
-- configuration template or environment, or that a specified solution stack
-- defines. The description includes the values the options, their default
-- values, and an indication of the required action on a running environment
-- if an option value is changed.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &TemplateName=default &Operation=DescribeConfigurationOptions &AuthParams
-- 32bit Amazon Linux running Tomcat 7 false RestartEnvironment 2000 ImageId
-- Scalar ami-6036c009 aws:autoscaling:launchconfiguration false
-- NoInterruption 2000 Notification Endpoint Scalar
-- aws:elasticbeanstalk:sns:topics false RestartApplicationServer 2000 PARAM4
-- Scalar aws:elasticbeanstalk:application:environment false
-- RestartApplicationServer 2000 JDBC_CONNECTION_STRING Scalar
-- aws:elasticbeanstalk:application:environment false RestartEnvironment 2000
-- SecurityGroups Scalar elasticbeanstalk-default
-- aws:autoscaling:launchconfiguration false NoInterruption 2
-- UnhealthyThreshold Scalar 5 10 aws:elb:healthcheck false RestartEnvironment
-- InstanceType t1.micro m1.small Scalar t1.micro
-- aws:autoscaling:launchconfiguration false NoInterruption Statistic Minimum
-- Maximum Sum Average Scalar Average aws:autoscaling:trigger false
-- RestartEnvironment LoadBalancerHTTPSPort OFF 443 8443 5443 Scalar OFF
-- aws:elb:loadbalancer false NoInterruption 0 Stickiness Cookie Expiration
-- Scalar 0 1000000 aws:elb:policies false RestartApplicationServer 2000
-- PARAM5 Scalar aws:elasticbeanstalk:application:environment false
-- NoInterruption MeasureName CPUUtilization NetworkIn NetworkOut DiskWriteOps
-- DiskReadBytes DiskReadOps DiskWriteBytes Latency RequestCount
-- HealthyHostCount UnhealthyHostCount Scalar NetworkOut
-- aws:autoscaling:trigger false NoInterruption 5 Interval Scalar 30 300
-- aws:elb:healthcheck false NoInterruption 2000 Application Healthcheck URL
-- Scalar / aws:elasticbeanstalk:application false NoInterruption 2000
-- Notification Topic ARN Scalar aws:elasticbeanstalk:sns:topics false
-- NoInterruption 2000 LowerBreachScaleIncrement Scalar -1
-- aws:autoscaling:trigger false RestartApplicationServer 2000 ^\S*$ nospaces
-- XX:MaxPermSize Scalar 64m aws:elasticbeanstalk:container:tomcat:jvmoptions
-- false NoInterruption 2000 UpperBreachScaleIncrement Scalar 1
-- aws:autoscaling:trigger false NoInterruption 1 MinSize Scalar 1 10000
-- aws:autoscaling:asg false RestartEnvironment Custom Availability Zones
-- us-east-1a us-east-1b us-east-1c us-east-1d List us-east-1a
-- aws:autoscaling:asg false RestartEnvironment Availability Zones Any 1 Any 2
-- Scalar Any 1 aws:autoscaling:asg false NoInterruption LogPublicationControl
-- Boolean false aws:elasticbeanstalk:hostmanager false
-- RestartApplicationServer 2000 JVM Options Scalar
-- aws:elasticbeanstalk:container:tomcat:jvmoptions false NoInterruption 2000
-- Notification Topic Name Scalar aws:elasticbeanstalk:sns:topics false
-- RestartApplicationServer 2000 PARAM2 Scalar
-- aws:elasticbeanstalk:application:environment false RestartEnvironment
-- LoadBalancerHTTPPort OFF 80 8080 Scalar 80 aws:elb:loadbalancer false
-- NoInterruption 2 Timeout Scalar 5 60 aws:elb:healthcheck false
-- NoInterruption 1 BreachDuration Scalar 2 600 aws:autoscaling:trigger false
-- RestartEnvironment MonitoringInterval 1 minute 5 minute Scalar 5 minute
-- aws:autoscaling:launchconfiguration false RestartApplicationServer 2000
-- PARAM1 Scalar aws:elasticbeanstalk:application:environment false
-- NoInterruption 1 MaxSize Scalar 4 10000 aws:autoscaling:asg false
-- NoInterruption 0 LowerThreshold Scalar 2000000 20000000
-- aws:autoscaling:trigger false RestartApplicationServer 2000 AWS_SECRET_KEY
-- Scalar aws:elasticbeanstalk:application:environment false
-- RestartApplicationServer 2000 AWS_ACCESS_KEY_ID Scalar
-- aws:elasticbeanstalk:application:environment false NoInterruption 0
-- UpperThreshold Scalar 6000000 20000000 aws:autoscaling:trigger false
-- NoInterruption Notification Protocol http https email email-json sqs Scalar
-- email aws:elasticbeanstalk:sns:topics false NoInterruption Unit Seconds
-- Percent Bytes Bits Count Bytes/Second Bits/Second Count/Second None Scalar
-- Bytes aws:autoscaling:trigger false RestartApplicationServer 2000 ^\S*$
-- nospaces Xmx Scalar 256m aws:elasticbeanstalk:container:tomcat:jvmoptions
-- false NoInterruption 0 Cooldown Scalar 360 10000 aws:autoscaling:asg false
-- NoInterruption 1 Period Scalar 1 600 aws:autoscaling:trigger false
-- RestartApplicationServer 2000 ^\S*$ nospaces Xms Scalar 256m
-- aws:elasticbeanstalk:container:tomcat:jvmoptions false RestartEnvironment
-- 2000 EC2KeyName Scalar aws:autoscaling:launchconfiguration false
-- NoInterruption Stickiness Policy Boolean false aws:elb:policies false
-- RestartApplicationServer 2000 PARAM3 Scalar
-- aws:elasticbeanstalk:application:environment false NoInterruption 2
-- HealthyThreshold Scalar 3 10 aws:elb:healthcheck false RestartEnvironment
-- 2000 SSLCertificateId Scalar aws:elb:loadbalancer
-- e8768900-f272-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk
    (
    -- * Request
      DescribeConfigurationOptions
    -- ** Request constructor
    , mkDescribeConfigurationOptions
    -- ** Request lenses
    , dcoApplicationName
    , dcoTemplateName
    , dcoEnvironmentName
    , dcoSolutionStackName
    , dcoOptions

    -- * Response
    , DescribeConfigurationOptionsResponse
    -- ** Response constructor
    , mkDescribeConfigurationOptionsResponse
    -- ** Response lenses
    , dcorSolutionStackName
    , dcorOptions
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | Result message containig a list of application version descriptions.
data DescribeConfigurationOptions = DescribeConfigurationOptions
    { _dcoApplicationName :: !(Maybe Text)
    , _dcoTemplateName :: !(Maybe Text)
    , _dcoEnvironmentName :: !(Maybe Text)
    , _dcoSolutionStackName :: !(Maybe Text)
    , _dcoOptions :: [OptionSpecification]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeConfigurationOptions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationName ::@ @Maybe Text@
--
-- * @TemplateName ::@ @Maybe Text@
--
-- * @EnvironmentName ::@ @Maybe Text@
--
-- * @SolutionStackName ::@ @Maybe Text@
--
-- * @Options ::@ @[OptionSpecification]@
--
mkDescribeConfigurationOptions :: DescribeConfigurationOptions
mkDescribeConfigurationOptions = DescribeConfigurationOptions
    { _dcoApplicationName = Nothing
    , _dcoTemplateName = Nothing
    , _dcoEnvironmentName = Nothing
    , _dcoSolutionStackName = Nothing
    , _dcoOptions = mempty
    }

-- | The name of the application associated with the configuration template or
-- environment. Only needed if you want to describe the configuration options
-- associated with either the configuration template or environment.
dcoApplicationName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoApplicationName =
    lens _dcoApplicationName (\s a -> s { _dcoApplicationName = a })

-- | The name of the configuration template whose configuration options you want
-- to describe.
dcoTemplateName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoTemplateName = lens _dcoTemplateName (\s a -> s { _dcoTemplateName = a })

-- | The name of the environment whose configuration options you want to
-- describe.
dcoEnvironmentName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoEnvironmentName =
    lens _dcoEnvironmentName (\s a -> s { _dcoEnvironmentName = a })

-- | The name of the solution stack whose configuration options you want to
-- describe.
dcoSolutionStackName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoSolutionStackName =
    lens _dcoSolutionStackName (\s a -> s { _dcoSolutionStackName = a })

-- | If specified, restricts the descriptions to only the specified options.
dcoOptions :: Lens' DescribeConfigurationOptions [OptionSpecification]
dcoOptions = lens _dcoOptions (\s a -> s { _dcoOptions = a })

instance ToQuery DescribeConfigurationOptions where
    toQuery = genericQuery def

-- | Describes the settings for a specified configuration set.
data DescribeConfigurationOptionsResponse = DescribeConfigurationOptionsResponse
    { _dcorSolutionStackName :: !(Maybe Text)
    , _dcorOptions :: [ConfigurationOptionDescription]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeConfigurationOptionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SolutionStackName ::@ @Maybe Text@
--
-- * @Options ::@ @[ConfigurationOptionDescription]@
--
mkDescribeConfigurationOptionsResponse :: DescribeConfigurationOptionsResponse
mkDescribeConfigurationOptionsResponse = DescribeConfigurationOptionsResponse
    { _dcorSolutionStackName = Nothing
    , _dcorOptions = mempty
    }

-- | The name of the solution stack these configuration options belong to.
dcorSolutionStackName :: Lens' DescribeConfigurationOptionsResponse (Maybe Text)
dcorSolutionStackName =
    lens _dcorSolutionStackName (\s a -> s { _dcorSolutionStackName = a })

-- | A list of ConfigurationOptionDescription.
dcorOptions :: Lens' DescribeConfigurationOptionsResponse [ConfigurationOptionDescription]
dcorOptions = lens _dcorOptions (\s a -> s { _dcorOptions = a })

instance FromXML DescribeConfigurationOptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeConfigurationOptions where
    type Sv DescribeConfigurationOptions = ElasticBeanstalk
    type Rs DescribeConfigurationOptions = DescribeConfigurationOptionsResponse

    request = post "DescribeConfigurationOptions"
    response _ = xmlResponse
