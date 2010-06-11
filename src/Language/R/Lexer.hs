{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, CPP, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.R.Lexer
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 05/28/10
-- 
-- Description :
--    This module contains the functions used for lexing an list of
--    strings (representing either source code lines or interactive
--    input) into a stack of tokens.
-----------------------------------------------------------------------------

module Language.R.Lexer where


import Language.R.SrcLocation
import Language.R.Token
import Language.R.AST

retokenize :: [Token] -> [TToken] -> Maybe [TToken]
retokenize [] outToks     = outToks
retokenize inToks outToks =
   let slurp     = takeWhile (\z -> tryBuildSyntaxNode z == []) inToks'
       inToks'   = drop (length slurp) inToks'
       newSyntax = buildSyntaxNode slurp
       outToks'  = concat [[newSyntax], outToks]
   in retokenize inToks' outToks'
  

data IdentStatus = IdReserved 
                 | IdBuiltin
                 | IdDefined
                 | IdUndefined


classifyIdent tk
  | tk `elem` (map fst rFuncPrims) = IdReserved 
  | tk `elem` rBuiltins            = IdBuiltin
  | tk `elem` (map fst rNameTable) = IdDefined
  | otherwise                      = IdUndefined

-- | Production rules
buildStmtExpr 















rBuiltins = 
 [ "zeroin2", "zeroin", "xzfile", "xspline", "X11", "wsbrowser" "writeLines"
 , "writeChar", "writeBin", "write.table", "withVisible", "which.min"
 , "which.max", "warning", "Version", "vector", "utf8ToInt", "url"
 , "update.formula", "unzip", "unz", "unserializeFromConn"
 , "unregisterNamespace", "unlockBinding", "unlist", "unlink", "unique"
 , "undebug", "typeof", "type.convert", "truncate", "toupper", "tolower"
 , "title", "textConnectionValue", "textConnection", "text", "terms.formula"
 , "tempfile", "tempdir", "tcrossprod", "t.default", "system", "Sys.unsetenv"
 , "Sys.umask", "Sys.time", "Sys.sleep", "Sys.setlocale", "Sys.setenv"
 , "Sys.readlink", "sys.parents", "sys.parent", "sys.on.exit", "sys.nframe"
 , "Sys.localeconv", "Sys.info", "Sys.glob", "Sys.getpid", "Sys.getlocale"
 , "Sys.getenv", "sys.function", "sys.frames", "sys.frame", "Sys.chmod"
 , "sys.calls", "sys.call", "symbols", "switch", "summary.connection"
 , "substr<-", "substr", "sub", "strwidth", "strtrim", "strsplit", "strptime"
 , "strheight", "stopHTTPD", "stop", "stdout", "stdin", "stderr", "startHTTPD"
 , "sprintf", "split", "sort", "sockSelect", "socketConnection", "sink.number"
 , "sink", "setwd", "setToCConverterActiveStatus", "setTimeLimit"
 , "setSessionTimeLimit", "seterrmessage", "setEncoding", "set.seed"
 , "serializeToConn", "select.list", "segments", "seek", "search", "scan"
 , "saveToConn", "savePlot", "savehistory", "save.to.file", "save", "sample"
 , "rwilcox", "rweibull", "runif", "rt", "rsignrank", "Rprofmem", "Rprof"
 , "rpois", "rowSums", "rowMeans", "row", "rnorm", "RNGkind", "rnchisq"
 , "rnbinom_mu", "rnbinom", "rmultinom", "rlogis", "rlnorm", "rhyper", "rgeom"
 , "rgb2hsv", "rgb256", "rgb", "rgamma", "rf", "rexp", "restart", "rep.int"
 , "removeToCConverterActiveStatus", "remove", "registerNamespace", "regexpr"
 , "reg.finalizer", "rect", "recordGraphics", "Recall", "readTableHead"
 , "readLines", "readline", "readDCF", "readChar", "readBin", "rchisq"
 , "rcauchy", "rbinom", "rbind", "rbeta", "rawToChar", "rawToBits", "rawShift"
 , "rawConnectionValue", "rawConnection", "rapply", "rank", "radixsort"
 , "R.home", "qwilcox", "qweibull", "qunif", "quit", "qtukey", "qt", "qsort"
 , "qsignrank", "qpois", "qnt", "qnorm", "qnf", "qnchisq", "qnbinom_mu"
 , "qnbinom", "qnbeta", "qlogis", "qlnorm", "qhyper", "qgeom", "qgamma", "qf"
 , "qexp", "qchisq", "qcauchy", "qbinom", "qbeta", "pwilcox", "pweibull"
 , "putconst", "pushBackLength", "pushBack", "punif", "ptukey", "pt", "psort"
 , "psignrank", "psigamma", "prmatrix", "printDeferredWarnings"
 , "print.function", "print.default", "ppois", "POSIXlt2Date", "polyroot"
 , "polygon", "pnt", "pnorm", "pnf", "pnchisq", "pnbinom_mu", "pnbinom"
 , "pnbeta", "pmin", "pmax", "pmatch", "plot.xy", "plot.window", "plot.new"
 , "plogis", "plnorm", "playSnapshot", "pkgbrowser", "pipe", "phyper", "pgeom"
 , "pgamma", "pf", "pexp", "persp", "pchisq", "pcauchy", "pbinom", "pbeta"
 , "path.expand", "paste", "parse_Rd", "parse", "parent.frame", "parent.env<-"
 , "parent.env", "par", "palette", "packBits", "package.manager", "order"
 , "options", "optimhess", "optim", "open", "object.size", "nsl"
 , "normalizePath", "nlm", "ngettext", "nextn", "NextMethod", "new.env", "nchar"
 , "mvfft", "mtext", "model.matrix", "model.frame", "mkUnbound", "mkCode"
 , "mget", "merge", "menu", "memory.profile", "memDecompress", "memCompress"
 , "mem.limits", "mean", "matrix", "match.call", "match", "makeLazy"
 , "makeActiveBinding", "make.unique", "make.names", "machine", "ls"
 , "lockEnvironment", "lockBinding", "locator", "loadhistory", "loadFromConn2"
 , "load.from.file", "load", "list.files", "lib.fixup", "lchoose", "lbeta"
 , "layout", "lapply", "l10n_info", "isSeekable", "isOpen", "isNamespaceEnv"
 , "islistfactor", "isIncomplete", "isdebugged", "is.vector", "is.unsorted"
 , "is.loaded", "is.builtin.internal", "intToUtf8", "intToBits"
 , "interruptsSuspended", "inspect", "inherits", "index.search", "importIntoEnv"
 , "image", "identify", "identical", "icuSetCollate", "iconv", "hsv"
 , "hsbrowser", "hcl", "gzfile", "gzcon", "gsub", "grepl", "grep", "gregexpr"
 , "grconvertY", "grconvertX", "gray", "getwd", "gettext", "getSnapshot"
 , "getRtoCConverterStatus", "getRtoCConverterDescriptions"
 , "getRegisteredNamespace", "getNumRtoCConverters", "getNamespaceRegistry"
 , "getGraphicsEvent", "geterrmessage", "getConnection", "getAllConnections"
 , "get", "gctorture", "gcinfo", "gc", "format.POSIXlt", "format.info", "format"
 , "formals", "fmin", "flush.console", "flush", "filledcontour", "file.symlink"
 , "file.show", "file.rename", "file.remove", "file.path", "file.info"
 , "file.exists", "file.edit", "file.create", "file.copy", "file.choose"
 , "file.append", "file.access", "file", "fifo", "fft", "exists"
 , "eval.with.vis", "eval", "erase", "environmentName", "environmentIsLocked"
 , "environment", "env2list", "env.profile", "Encoding", "encodeString", "edit"
 , "eapply", "dyn.unload", "dyn.load", "dwilcox", "dweibull", "duplicated"
 , "dunif", "dump", "dtukey", "dt", "dsignrank", "drop", "dput", "dpois"
 , "download", "do.call", "dnt", "dnorm", "dnf", "dnchisq", "dnbinom_mu"
 , "dnbinom", "dnbeta", "dlogis", "dlnorm", "disassemble", "dirname", "dirchmod"
 , "dir.create", "dhyper", "dgeom", "dgamma", "df", "dexp", "devAskNewPage"
 , "dev.size", "dev.set", "dev.prev", "dev.off", "dev.next", "dev.displaylist"
 , "dev.cur", "dev.copy", "dev.control", "detach", "deriv.default", "deparseRd"
 , "deparse", "dend.window", "dend", "delayedAssign", "debugonce", "debug"
 , "dchisq", "dcauchy", "dbinom", "dbeta", "Date2POSIXlt", "date", "dataviewer"
 , "dataentry", "data.manager", "D", "Cstack_info", "crossprod", "cov"
 , "count.fields", "cor", "contourLines", "contour", "complex", "complete.cases"
 , "comment<-", "comment", "commandArgs", "colSums", "colors", "colMeans"
 , "col2rgb", "col", "codeFiles.append", "close", "clip", "clearPushBack"
 , "choose", "chartr", "charToRaw", "charmatch", "cbind", "cat"
 , "capabilitiesX11", "capabilities", "cairo", "bzfile", "builtins"
 , "browserText", "browserSetDebug", "browserCondition", "box", "bodyCode"
 , "body", "bindtextdomain", "bindingIsLocked", "bindingIsActive", "beta"
 , "besselY", "besselK", "besselJ", "besselI", "bcVersion", "bcClose"
 , "basename", "axis", "attach", "atan2", "assign", "as.vector", "as.POSIXlt"
 , "as.POSIXct", "as.function.default", "arrows", "args", "aqua.custom.print"
 , "aperm", "anyDuplicated", "all.names", "agrep", "addhistory", "abline"
 , "abbreviate", ".signalCondition", ".resetCondHands", ".invokeRestart"
 , ".getRestart", ".dfltWarn", ".dfltStop", ".addTryHandlers", ".addRestart"
 , ".addCondHands" 
 ]

