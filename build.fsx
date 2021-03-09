#load "node_modules/fable-publish-utils/PublishUtils.fs"
open PublishUtils

match argsLower with
| IgnoreCase "publish"::_rest -> pushNuget "./Fable.TsDeclaration.fsproj" [] doNothing
| _ -> ()