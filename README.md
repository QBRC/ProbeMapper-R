# ProbeMapper R Package

A REST-based R client for ProbeMapper.

The ProbeMapper web service now utilizes [JSON Table Schema](http://www.dataprotocols.org/en/latest/json-table-schema.html) to transmit data. We use the [RODProt](https://github.com/QBRC/RODProt) package to digest this data in R.

## Installation & Usage

Until the package is made available on CRAN, you'll likely want to use the [devtools](https://github.com/hadley/devtools) R package to install this package from this GitHub repo:

	library(devtools)
	#Until RODProt is available on CRAN, you'll need to install that dependency yourself.
	install_github("RODProt", "QBRC")

	install_github("ProbeMappper-R", "QBRC")
	library(probemapper)
    
    
Following installation, you'll likely need to define a "Getter" which will handle all of the communication with the web service for you. By default, a basic `HTTPGetter` will be created which would allow you to access an unauthenticated ProbeMapper web service. By default, however, we secure our web services using [HMAC](http://en.wikipedia.org/wiki/Hash-based_message_authentication_code), which means the basic `HTTPGetter` will not be able to make any authenticated requests. Otherwise, you'll get an error:

    > get_authority()
    Error in function (type, msg, asError = TRUE)  : 
      Received HTTP code 502 from proxy after CONNECT

You will need to define an `HMACGetter` in order to make valid requests. You can retrieve your user ID and secret key from QBRC. Once you have these credentials, you can create your `HMACGetter` as follows:

    getter <- HMACGetter$new(id="some user", key="<user key here>")

(substituting in your actual `id` and `key`). This will allow you to make authenticated requests:

    > get_authority(getter=getter)
      id        title
    1  1        BLAST
    2  2       Vendor
    3  3 Bioconductor

For convenience, the default `getter` which will be used in all ProbeMapper requests will be pulled in from `getOption("pmGetteR")`, if defined. You can set this option manually to avoid having to specify the getter for each request:

    options(pmGetter = getter)
    > get_authority()
      id        title
    1  1        BLAST
    2  2       Vendor
    3  3 Bioconductor
    
## Development/Testing

If you're looking to make modifications to the package, you may notice that, when testing, all the tests fail. This is likey due to the fact that you don't have an `HMACGetter` defined. Without such a getter, `probemapper` will attempt to make unauthenticated requests to the web service, which will likely fail (see the section above for a discussion about HMAC and security). In order to have the tests succeed, you'll need to create a file at `inst/tests/setGetter.R` which follows the following template:

    getter <- HMACGetter$new(id="some user", key="<user key here>")
    options(pmGetter = getter)
    
Note that you can copy `inst/tests/setGetter.example.R`, if you desire. You'll need to specify the `id` and `key` fields above in order to begin making valid HMAC requests, which will allow the tests to pass.