package zd
package proto

import org.scalatest.{FreeSpec, Matchers}
import zd.proto.api.{N}

class PurescriptSpec extends FreeSpec with Matchers {
  val res2 = Purescript.generate[Push, Pull](moduleEncodeName="Pull", moduleDecodeName="Push", "Common")
  "purs has" - {
    "module name" in {
      res2.encode.prelude.startsWith("module Pull")
      res2.decode.prelude.startsWith("module Push")
    }
    "types decode" in {
      val res = res2.decode
      res.types.length shouldBe 14
      res.types(0) shouldBe "data Push = SiteOpts SiteOpts | Permissions Permissions | Page Page | PageTreeItem PageTreeItem"
      res.types(1) shouldBe "type SiteOpts = { xs :: Array SiteOpt }"
      res.types(2) shouldBe "type SiteOpts' = { xs :: Array SiteOpt }"
      res.types(3) shouldBe "type SiteOpt = { id :: String, label :: Maybe String }"
      res.types(4) shouldBe "type SiteOpt' = { id :: Maybe String, label :: Maybe String }"
      res.types(5) shouldBe "type Permissions = { xs :: Array String }"
      res.types(6) shouldBe "type Permissions' = { xs :: Array String }"
      res.types(7) shouldBe "type Page = { tpe :: PageType, guest :: Boolean, seo :: PageSeo, mobileSeo :: Maybe PageSeo, name :: Map String String }"
      res.types(8) shouldBe "type Page' = { tpe :: Maybe PageType, guest :: Maybe Boolean, seo :: Maybe PageSeo, mobileSeo :: Maybe PageSeo, name :: Map String String }"
      res.types(9) shouldBe "type PageWidgets' = {  }"
      res.types(10) shouldBe "type PageUrl' = { addr :: Maybe String }"
      res.types(11) shouldBe "type PageSeo' = { descr :: Maybe String, order :: Maybe Number }"
      res.types(12) shouldBe "type PageTreeItem = { priority :: Int }"
      res.types(13) shouldBe "type PageTreeItem' = { priority :: Maybe Int }"
    }
    "types encode" in {
      val res = res2.encode
      res.types.length shouldBe 4
      res.types(0) shouldBe "data Pull = GetSites GetSites | UploadChunk UploadChunk | SavePage SavePage"
      res.types(1) shouldBe "type GetSites = {  }"
      res.types(2) shouldBe "type UploadChunk = { path :: Array String, id :: String, chunk :: Uint8Array }"
      ()
    }
    "decoders" in {
      val xs = res2.decode.coders
      xs(0) should be (Snippets.decodePush)
      xs(2) should be (Snippets.decodeSiteOpt)
      xs(1) should be (Snippets.decodeSiteOpts)
      xs(3) should be (Snippets.decodePermissions)
      xs(5) should be (Snippets.decodePageType)
      xs(6) should be (Snippets.decodePageWidgets)
      xs(7) should be (Snippets.decodePageUrl)
      xs(4) should be (Snippets.decodePage)
      xs(9) should be (Snippets.decodePageTreeItem)
    }
    "encoders" in {
      res2.encode.coders.foreach{
        case e if e.startsWith("encodePull :: ") => e should be (Snippets.encodePull)
        case e if e.startsWith("encodeGetSites :: ") => e should be (Snippets.encodeGetSites)
        case e if e.startsWith("encodeUploadChunk :: ") => e should be (Snippets.encodeUploadChunk)
        case _ =>
      }
    }
    "print" in {
      println(Res.format(res2.common))
      Res.writeToFile("purs/test/src/Common.purs", res2.common)
      println(Res.format(res2.decode))
      Res.writeToFile("purs/test/src/Push.purs", res2.decode)
      println(Res.format(res2.encode))
      Res.writeToFile("purs/test/src/Pull.purs", res2.encode)
    }
  }
}

sealed trait Push
@N(1) final case class SiteOpts(@N(1) xs: Stream[SiteOpt]) extends Push
@N(2) final case class Permissions(@N(1) xs: List[String]) extends Push
@N(3) final case class Page(@N(1) tpe: PageType, @N(2) guest: Boolean, @N(3) seo: PageSeo, @N(4) mobileSeo: Option[PageSeo], @N(5) name: Map[String,String]) extends Push
final case class PageSeo(@N(1) descr: String, @N(2) order: Double)
@N(4) final case class PageTreeItem(@N(1) priority: Int) extends Push

final case class SiteOpt(@N(1) id: String, @N(2) label: Option[String])
sealed trait PageType
@N(1) final case class PageWidgets() extends PageType
@N(2) final case class PageUrl(@N(1) addr: String) extends PageType

sealed trait Pull
@N(1000) final case class GetSites() extends Pull
@N(1001) final case class UploadChunk
  ( @N(1) path: List[String]
  , @N(2) id: String
  , @N(3) chunk: Array[Byte]
  ) extends Pull
@N(1002) final case class SavePage(@N(1) tpe: PageType, @N(2) guest: Boolean, @N(3) seo: PageSeo, @N(4) mobileSeo: Option[PageSeo], @N(5) name: Map[String,String]) extends Pull

object Snippets {
  val decodePush = """decodePush :: Uint8Array -> Decode.Result Push
decodePush _xs_ = do
  { pos: pos1, val: tag } <- Decode.uint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> do
      { pos: pos2, val } <- decodeSiteOpts _xs_ pos1
      pure { pos: pos2, val: SiteOpts val }
    2 -> do
      { pos: pos2, val } <- decodePermissions _xs_ pos1
      pure { pos: pos2, val: Permissions val }
    3 -> do
      { pos: pos2, val } <- decodePage _xs_ pos1
      pure { pos: pos2, val: Page val }
    4 -> do
      { pos: pos2, val } <- decodePageTreeItem _xs_ pos1
      pure { pos: pos2, val: PageTreeItem val }
    i ->
      Left $ Decode.BadType i"""

  val decodeSiteOpt = """decodeSiteOpt :: Uint8Array -> Int -> Decode.Result SiteOpt
decodeSiteOpt _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { id: Nothing, label: Nothing } pos
  case val of
    { id: Just id, label } -> pure { pos: pos1, val: { id, label } }
    _ -> Left $ Decode.MissingFields "SiteOpt"
    where
    decode :: Int -> SiteOpt' -> Int -> Decode.Result SiteOpt'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { id = Just val }) pos3
              2 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { label = Just val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val decodeSiteOpts = """decodeSiteOpts :: Uint8Array -> Int -> Decode.Result SiteOpts
decodeSiteOpts _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { xs: [] } pos
  case val of
    { xs } -> pure { pos: pos1, val: { xs } }
    where
    decode :: Int -> SiteOpts' -> Int -> Decode.Result SiteOpts'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case decodeSiteOpt _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { xs = snoc acc.xs val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val decodePermissions = """decodePermissions :: Uint8Array -> Int -> Decode.Result Permissions
decodePermissions _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { xs: [] } pos
  case val of
    { xs } -> pure { pos: pos1, val: { xs } }
    where
    decode :: Int -> Permissions' -> Int -> Decode.Result Permissions'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { xs = snoc acc.xs val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val decodePage = """decodePage :: Uint8Array -> Int -> Decode.Result Page
decodePage _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { tpe: Nothing, guest: Nothing, seo: Nothing, mobileSeo: Nothing, name: Map.empty } pos
  case val of
    { tpe: Just tpe, guest: Just guest, seo: Just seo, mobileSeo, name } -> pure { pos: pos1, val: { tpe, guest, seo, mobileSeo, name } }
    _ -> Left $ Decode.MissingFields "Page"
    where
    decode :: Int -> Page' -> Int -> Decode.Result Page'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case decodePageType _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { tpe = Just val }) pos3
              2 ->
                case Decode.boolean _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { guest = Just val }) pos3
              3 ->
                case decodePageSeo _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { seo = Just val }) pos3
              4 ->
                case decodePageSeo _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { mobileSeo = Just val }) pos3
              5 ->
                case decodeStringString _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { name = Map.insert val.first val.second acc.name }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val decodePageType = """decodePageType :: Uint8Array -> Int -> Decode.Result PageType
decodePageType _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  decode end Nothing pos
    where
    decode :: Int -> Maybe PageType -> Int -> Decode.Result PageType
    decode end acc pos1 | pos1 < end =
      case Decode.uint32 _xs_ pos1 of
        Left x -> Left x
        Right { pos: pos2, val: tag } ->
          case tag `zshr` 3 of
            1 ->
              case decodePageWidgets _xs_ pos2 of
                Left x -> Left x
                Right { pos: pos3, val } ->
                  decode end (Just $ PageWidgets val) pos3
            2 ->
              case decodePageUrl _xs_ pos2 of
                Left x -> Left x
                Right { pos: pos3, val } ->
                  decode end (Just $ PageUrl val) pos3
            _ ->
              case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                Left x -> Left x
                Right { pos: pos3 } ->
                  decode end acc pos3
    decode end (Just acc) pos1 = pure { pos: pos1, val: acc }
    decode end acc@Nothing pos1 = Left $ Decode.MissingFields "PageType""""

  val decodePageUrl = """decodePageUrl :: Uint8Array -> Int -> Decode.Result PageUrl
decodePageUrl _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { addr: Nothing } pos
  case val of
    { addr: Just addr } -> pure { pos: pos1, val: { addr } }
    _ -> Left $ Decode.MissingFields "PageUrl"
    where
    decode :: Int -> PageUrl' -> Int -> Decode.Result PageUrl'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.string _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { addr = Just val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val decodePageWidgets = """decodePageWidgets :: Uint8Array -> Int -> Decode.Result PageWidgets
decodePageWidgets _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end {  } pos
  case val of
    {  } -> pure { pos: pos1, val: {  } }
    where
    decode :: Int -> PageWidgets' -> Int -> Decode.Result PageWidgets'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val decodePageTreeItem = """decodePageTreeItem :: Uint8Array -> Int -> Decode.Result PageTreeItem
decodePageTreeItem _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  let end = pos + msglen
  { pos: pos1, val } <- decode end { priority: Nothing } pos
  case val of
    { priority: Just priority } -> pure { pos: pos1, val: { priority } }
    _ -> Left $ Decode.MissingFields "PageTreeItem"
    where
    decode :: Int -> PageTreeItem' -> Int -> Decode.Result PageTreeItem'
    decode end acc pos1 =
      if pos1 < end then
        case Decode.uint32 _xs_ pos1 of
          Left x -> Left x
          Right { pos: pos2, val: tag } ->
            case tag `zshr` 3 of
              1 ->
                case Decode.int32 _xs_ pos2 of
                  Left x -> Left x
                  Right { pos: pos3, val } ->
                    decode end (acc { priority = Just val }) pos3
              _ ->
                case Decode.skipType _xs_ pos2 $ tag .&. 7 of
                  Left x -> Left x
                  Right { pos: pos3 } ->
                    decode end acc pos3
      else pure { pos: pos1, val: acc }"""

  val encodePull = """encodePull :: Pull -> Uint8Array
encodePull (GetSites x) = concatAll [ Encode.uint32 8002, encodeGetSites x ]
encodePull (UploadChunk x) = concatAll [ Encode.uint32 8010, encodeUploadChunk x ]
encodePull (SavePage x) = concatAll [ Encode.uint32 8018, encodeSavePage x ]"""

  val encodeGetSites = """encodeGetSites :: GetSites -> Uint8Array
encodeGetSites _ = Encode.uint32 0"""

  val encodeUploadChunk = """encodeUploadChunk :: UploadChunk -> Uint8Array
encodeUploadChunk msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.uint32 10, Encode.string x ]) msg.path
        , Encode.uint32 18
        , Encode.string msg.id
        , Encode.uint32 26
        , Encode.bytes msg.chunk
        ]
  let len = length xs
  concatAll [ Encode.uint32 len, xs ]"""
}
