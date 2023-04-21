%%% @doc
%%% The Vanillae Erlang Interface to Aeternity
%%%
%%% This module is the high-level interface to the Aeternity blockchain system.
%%% The interface is split into three main sections:
%%% - Get/Set admin functions
%%% - AE node JSON query interface functions
%%% - AE contract call and serialization interface functions
%%%
%%% The get/set admin functions are for setting or checking things like the Aeternity
%%% "network ID" and list of addresses of AE nodes you want to use for answering
%%% queries to the blockchain (usually you will run these nodes in your own back end).
%%%
%%% The JSON query interface functions are the blockchain query functions themselves
%%% which are translated to network queries and return Erlang messages as responses.
%%%
%%% The contract call and serialization interface are the functions used to convert
%%% a desired call to a smart contract on the chain to call data serialized in a form
%%% that an Aeternity compatible wallet, SDK or (in the case of a web service) in-page
%%% code based on a JS library such as Sidekick (another component of Vanillae) can
%%% use to generate signature requests and signed transaction objects for submission
%%% to an Aeternity network node for inclusion in the transaction mempool.
%%%
%%% This module also includes the standard OTP "application" interface and start/stop
%%% helper functions.
%%% @end

-module(vanillae).
-vsn("0.1.0").
%-behavior(application).
-author("Craig Everett <ceverett@tsuriai.jp>").
-copyright("Craig Everett <ceverett@tsuriai.jp>").
-license("GPL-3.0-or-later").

% Get/Set admin functions.
-export([network_id/0, network_id/1,
         ae_nodes/0,   ae_nodes/1,
         timeout/0,    timeout/1]).

% AE node JSON query interface functions
-export([top_height/0, top_block/0,
         kb_current/0, kb_current_hash/0, kb_current_height/0,
         kb_pending/0,
         kb_by_hash/1, kb_by_height/1,
%        kb_insert/1,
         mb_header/1, mb_txs/1, mb_tx_index/2, mb_tx_count/1,
         gen_current/0, gen_by_id/1, gen_by_height/1,
         acc/1, acc_at_height/2, acc_at_block_id/2,
         acc_pending_txs/1,
         next_nonce/1,
         dry_run/1, dry_run/2, dry_run/3,
         tx/1, tx_info/1,
         post_tx/1,
         contract/1, contract_code/1,
         contract_poi/1,
%        oracle/1, oracle_queries/1, oracle_queries_by_id/2,
         name/1,
%        channel/1,
         peer_pubkey/0,
         status/0,
         status_chainends/0]).

% AE contract call and serialization interface functions
-export([read_aci/1,
         min_gas/0,
         min_gas_price/0,
         min_fee/0,
         contract_create/3,
         contract_create/8,
         prepare_contract/1,
         contract_call/5,
         contract_call/6,
         contract_call/10,
         verify_signature/3]).


% OTP Application Interface
-export([start/0, stop/0]).
-export([start/2, stop/1]).


%%% Types

-export_type([ae_node/0, network_id/0, ae_error/0]).


-type ae_node()             :: {inet:ip_address(), inet:port_number()}.
-type network_id()          :: string().
-type ae_error()            :: not_started
                             | no_nodes
                             | timeout
                             | {timeout, Received :: binary()}
                             | inet:posix()
                             | {received, binary()}
                             | headers
                             | {headers, map()}
                             | bad_length
                             | gc_out_of_range.
-type pubkey()              :: unicode:chardata().  % "ak_" ++ _
-type account_id()          :: pubkey().
-type contract_id()         :: unicode:chardata().  % "ct_" ++ _
-type peer_pubkey()         :: string().            % "pp_" ++ _
-type keyblock_hash()       :: string().            % "kh_" ++ _
-type contract_byte_array() :: string().            % "cb_" ++ _
-type microblock_hash()     :: string().            % "mh_" ++ _
%-type block_state_hash()    :: string().            % "bs_" ++ _
%-type proof_of_fraud_hash() :: string() | no_fraud. % "bf_" ++ _
%-type signature()           :: string().            % "sg_" ++ _
%-type block_tx_hash()       :: string().            % "bx_" ++ _
-type tx_hash()             :: string().            % "th_" ++ _
%-type name_hash()           :: string().            % "nm_" ++ _
%-type protocol_info()       :: #{string() => term()}.
% #{"effective_at_height" => non_neg_integer(),
%   "version"             => pos_integer()}.
-type keyblock()            :: #{string() => term()}.
% #{"beneficiary"   => account_id(),
%   "hash"          => keyblock_hash(),
%   "height"        => pos_integer(),
%   "info"          => contract_byte_array(),
%   "miner"         => account_id(),
%   "nonce"         => non_neg_integer(),
%   "pow"           => [non_neg_integer()],
%   "prev_hash"     => microblock_hash(),
%   "prev_key_hash" => keyblock_hash(),
%   "state_hash"    => block_state_hash(),
%   "target"        => non_neg_integer(),
%   "time"          => non_neg_integer(),
%   "version"       => 5}.
-type microblock_header()   :: #{string() => term()}.
% #{"hash"          => microblock_hash(),
%   "height"        => pos_integer(),
%   "pof_hash"      => proof_of_fraud_hash(),
%   "prev_hash"     => microblock_hash() | keyblock_hash(),
%   "prev_key_hash" => keyblock_hash(),
%   "signature"     => signature(),
%   "state_hash"    => block_state_hash(),
%   "time"          => non_neg_integer(),
%   "txs_hash"      => block_tx_hash(),
%   "version"       => 1}.
-type transaction()         :: #{string() => term()}.
% #{"block_hash"    => microblock_hash(),
%   "block_height"  => pos_integer(),
%   "hash"          => tx_hash(),
%   "signatures"    => [signature()],
%   "tx"            =>
%       #{"abi_version" => pos_integer(),
%         "amount"      => non_neg_integer(),
%         "call_data"   => contract_byte_array(),
%         "code"        => contract_byte_array(),
%         "deposit"     => non_neg_integer(),
%         "fee"         => pos_integer(),
%         "gas"         => pos_integer(),
%         "gas_price"   => pos_integer(),
%         "nonce"       => pos_integer(),
%         "owner_id"    => account_id(),
%         "type"        => string(),
%         "version"     => pos_integer(),
%         "vm_version"  => pos_integer()}}
-type generation()          :: #{string() => term()}.
% #{"key_block"     => keyblock(),
%   "micro_blocks"  => [microblock_hash()]}.
-type account()             :: #{string() => term()}.
% #{"balance" => non_neg_integer(),
%   "id"      => account_id(),
%   "kind"    => "basic",
%   "nonce"   => pos_integer(),
%   "payable" => true}.
-type contract_data()       :: #{string() => term()}.
% #{"abi_version " => pos_integer(),
%   "active"       => boolean(),
%   "deposit"      => non_neg_integer(),
%   "id"           => contract_id(),
%   "owner_id"     => account_id() | contract_id(),
%   "referrer_ids" => [],
%   "vm_version"   => pos_integer()}.
-type name_info()           :: #{string() => term()}.
% #{"id"       => name_hash(),
%   "owner"    => account_id(),
%   "pointers" => [],
%   "ttl"      => non_neg_integer()}.
-type status()              :: #{string() => term()}.
% #{"difficulty"                 => non_neg_integer(),
%   "genesis_key_block_hash"     => keyblock_hash(),
%   "listening"                  => boolean(),
%   "network_id"                 => string(),
%   "node_revision"              => string(),
%   "node_version"               => string(),
%   "peer_connections"           => #{"inbound"  => non_neg_integer(),
%                                     "outbound" => non_neg_integer()},
%   "peer_count"                 => non_neg_integer(),
%   "peer_pubkey"                => peer_pubkey(),
%   "pending_transactions_count" => 51,
%   "protocols"                  => [protocol_info()],
%   "solutions"                  => non_neg_integer(),
%   "sync_progress"              => float(),
%   "syncing"                    => boolean(),
%   "top_block_height"           => non_neg_integer(),
%   "top_key_block_hash"         => keyblock_hash()}.




%%% Get/Set admin functions

-spec network_id() -> NetworkID
    when NetworkID :: string() | none.
%% @doc
%% Returns the AE network ID or the atom `none' if it is unset.
%% Checking this is not normally necessary, but if network ID assignment is dynamic
%% in your system it may be necessary to call this before attempting to form
%% call data or perform other actions on chain that require a signature.

network_id() ->
    vanillae_man:network_id().


-spec network_id(Identifier) -> ok | {error, Reason}
    when Identifier :: string() | none,
         Reason     :: not_started.
%% @doc
%% Sets the network ID, or returns `not_started' if the service is not yet started.

network_id(Identifier) ->
    vanillae_man:network_id(Identifier).


-spec ae_nodes() -> [ae_node()].
%% @doc
%% Returns the list of currently assigned nodes.
%% The normal reason to call this is in preparation for altering the nodes list or
%% checking the current list in debugging.

ae_nodes() ->
    vanillae_man:ae_nodes().


-spec ae_nodes(List) -> ok | {error, Reason}
    when List   :: [ae_node()],
         Reason :: {invalid, [term()]}.
%% @doc
%% Sets the AE nodes that are intended to be used as your interface to the AE peer
%% network. The common situation is that your project runs a non-mining AE node as
%% part of your backend infrastructure. Typically one or two nodes is plenty, but
%% this may need to expand depending on how much query load your application generates.
%% The Vanillae manager will load balance by round-robin distribution.

ae_nodes(List) ->
    vanillae_man:ae_nodes(List).


-spec timeout() -> Timeout
    when Timeout :: pos_integer() | infinity.
%% @doc
%% Returns the current request timeout setting in milliseconds.

timeout() ->
    vanillae_man:timeout().


-spec timeout(MS) -> ok
    when MS :: pos_integer() | infinity.
%% @doc
%% Sets the request timeout in milliseconds.

timeout(MS) ->
    vanillae_man:timeout(MS).



%%% AE node JSON query interface functions


-spec top_height() -> {ok, Height} | {error, Reason}
    when Height :: pos_integer(),
         Reason :: ae_error().

top_height() ->
    case top_block() of
        {ok, #{"micro_block" := #{"height" := Height}}} -> {ok, Height};
        {ok, #{"key_block"   := #{"height" := Height}}} -> {ok, Height};
        Error                                           -> Error
    end.


-spec top_block() -> {ok, TopBlock} | {error, Reason}
    when TopBlock :: #{Type := Block},
         Type     :: string(), % "key_block" | "micro_block"
         Block    :: keyblock() | microblock_header(),
         Reason   :: ae_error().
%% @doc
%% Returns the current block height as an integer.

top_block() ->
    request("/v2/blocks/top").


-spec kb_current() -> {ok, CurrentBlock} | {error, Reason}
    when CurrentBlock :: keyblock(),
         Reason       :: ae_error().
%% @doc
%% Returns the current keyblock's metadata as a map.

kb_current() ->
    request("/v2/key-blocks/current").


-spec kb_current_hash() -> {ok, Hash} | {error, Reason}
    when Hash   :: keyblock_hash(),
         Reason :: ae_error().
%% @doc
%% Returns the current keyblock's hash.
%% Equivalent of calling:
%% ```
%%   {ok, Current} = kb_current(),
%%   maps:get("hash", Current),
%% '''

kb_current_hash() ->
    case request("/v2/key-blocks/current/hash") of
        {ok, #{"reason" := Reason}} -> {error, Reason};
        {ok, #{"hash" := Hash}}     -> {ok, Hash};
        Error                       -> Error
    end.


-spec kb_current_height() -> {ok, Height} | {error, Reason}
    when Height :: pos_integer(),
         Reason :: ae_error() | string().
%% @doc
%% Returns the current keyblock's height as an integer.
%% Equivalent of calling:
%% ```
%%   {ok, Current} = kb_current(),
%%   maps:get("height", Current),
%% '''

kb_current_height() ->
    case request("/v2/key-blocks/current/height") of
        {ok, #{"reason" := Reason}} -> {error, Reason};
        {ok, #{"height" := Height}} -> {ok, Height};
        Error                       -> Error
    end.


-spec kb_pending() -> {ok, keyblock_hash()} | {error, Reason}
    when Reason :: string().
%% @doc
%% Request the hash of the pending keyblock of a mining node's beneficiary.
%% If the node queried is not configured for mining it will return
%%  `{error, "Beneficiary not configured"}'

kb_pending() ->
    result(request("/v2/key-blocks/pending")).


-spec kb_by_hash(ID) -> {ok, KeyBlock} | {error, Reason}
    when ID       :: keyblock_hash(),
         KeyBlock :: keyblock(),
         Reason   :: ae_error() | string().
%% @doc
%% Returns the keyblock identified by the provided hash.

kb_by_hash(ID) ->
    result(request(["/v2/key-blocks/hash/", ID])).


-spec kb_by_height(Height) -> {ok, KeyBlock} | {error, Reason}
    when Height   :: non_neg_integer(),
         KeyBlock :: keyblock(),
         Reason   :: ae_error() | string().
%% @doc
%% Returns the keyblock identigied by the provided height.

kb_by_height(Height) ->
    StringN = integer_to_list(Height),
    result(request(["/v2/key-blocks/height/", StringN])).


%kb_insert(KeyblockData) ->
%    request("/v2/key-blocks", KeyblockData).


-spec mb_header(ID) -> {ok, MB_Header} | {error, Reason}
    when ID        :: microblock_hash(),
         MB_Header :: microblock_header(),
         Reason    :: ae_error() | string().
%% @doc
%% Returns the header of the microblock indicated by the provided ID (hash).

mb_header(ID) ->
    result(request(["/v2/micro-blocks/hash/", ID, "/header"])).


-spec mb_txs(ID) -> {ok, TXs} | {error, Reason}
    when ID     :: microblock_hash(),
         TXs    :: [transaction()],
         Reason :: ae_error() | string().
%% @doc
%% Returns a list of transactions included in the microblock.

mb_txs(ID) ->
    case request(["/v2/micro-blocks/hash/", ID, "/transactions"]) of
        {ok, #{"transactions" := TXs}} -> {ok, TXs};
        {ok, #{"reason" := Reason}}    -> {error, Reason};
        Error                          -> Error
    end.


-spec mb_tx_index(MicroblockID, Index) -> {ok, TX} | {error, Reason}
    when MicroblockID :: microblock_hash(),
         Index        :: pos_integer(),
         TX           :: transaction(),
         Reason       :: ae_error() | string().
%% @doc
%% Retrieve a single transaction from a microblock by index.
%% (Note that indexes start from 1, not zero.)

mb_tx_index(ID, Index) ->
    StrHeight = integer_to_list(Index),
    result(request(["/v2/micro-blocks/hash/", ID, "/transactions/index/", StrHeight])).


-spec mb_tx_count(ID) -> {ok, Count} | {error, Reason}
    when ID     :: microblock_hash(),
         Count  :: non_neg_integer(),
         Reason :: ae_error() | string().
%% @doc
%% Retrieve the number of transactions contained in the indicated microblock.

mb_tx_count(ID) ->
    case request(["/v2/micro-blocks/hash/", ID, "/transactions/count"]) of
        {ok, #{"count" := Count}}   -> {ok, Count};
        {ok, #{"reason" := Reason}} -> {error, Reason};
        Error                       -> Error
    end.


-spec gen_current() -> {ok, Generation} | {error, Reason}
    when Generation :: generation(),
         Reason     :: ae_error() | string().
%% @doc
%% Retrieve the generation data (keyblock and list of associated microblocks) for
%% the current generation.

gen_current() ->
    result(request("/v2/generations/current")).


-spec gen_by_id(ID) -> {ok, Generation} | {error, Reason}
    when ID         :: keyblock_hash(),
         Generation :: generation(),
         Reason     :: ae_error() | string().
%% @doc
%% Retrieve generation data (keyblock and list of associated microblocks) by keyhash.

gen_by_id(ID) ->
    result(request(["/v2/generations/hash/", ID])).


-spec gen_by_height(Height) -> {ok, Generation} | {error, Reason}
    when Height     :: non_neg_integer(),
         Generation :: generation(),
         Reason     :: ae_error() | string().
%% @doc
%% Retrieve generation data (keyblock and list of associated microblocks) by height.

gen_by_height(Height) ->
    StrHeight = integer_to_list(Height),
    result(request(["/v2/generations/height/", StrHeight])).


-spec acc(AccountID) -> {ok, Account} | {error, Reason}
    when AccountID :: account_id(),
         Account   :: account(),
         Reason    :: ae_error() | string().
%% @doc
%% Retrieve account data by account ID (public key).

acc(AccountID) ->
    result(request(["/v2/accounts/", AccountID])).


-spec acc_at_height(AccountID, Height) -> {ok, Account} | {error, Reason}
    when AccountID :: account_id(),
         Height    :: non_neg_integer(),
         Account   :: account(),
         Reason    :: ae_error() | string().
%% @doc
%% Retrieve data for an account as that account existed at the given height.

acc_at_height(AccountID, Height) ->
    StrHeight = integer_to_list(Height),
    case request(["/v2/accounts/", AccountID, "/height/", StrHeight]) of
        {ok, #{"reason" := "Internal server error"}} -> {error, gc_out_of_range};
        {ok, #{"reason" := Reason}}                  -> {error, Reason};
        Result                                       -> Result
    end.


-spec acc_at_block_id(AccountID, BlockID) -> {ok, Account} | {error, Reason}
    when AccountID :: account_id(),
         BlockID   :: keyblock_hash() | microblock_hash(),
         Account   :: account(),
         Reason    :: ae_error() | string().
%% @doc
%% Retrieve data for an account as that account existed at the moment the given
%% block represented the current state of the chain.

acc_at_block_id(AccountID, BlockID) ->
    case request(["/v2/accounts/", AccountID, "/hash/", BlockID]) of
        {ok, #{"reason" := "Internal server error"}} -> {error, gc_out_of_range};
        {ok, #{"reason" := Reason}}                  -> {error, Reason};
        Result                                       -> Result
    end.


-spec acc_pending_txs(AccountID) -> {ok, TXs} | {error, Reason}
    when AccountID :: account_id(),
         TXs       :: [tx_hash()],
         Reason    :: ae_error() | string().
%% @doc
%% Retrieve a list of transactions pending for the given account.

acc_pending_txs(AccountID) ->
    request(["/v2/accounts/", AccountID, "/transactions/pending"]).


-spec next_nonce(AccountID) -> {ok, Nonce} | {error, Reason}
    when AccountID :: account_id(),
         Nonce     :: non_neg_integer(),
         Reason    :: ae_error() | string().
%% @doc
%% Retrieve the next nonce for the given account

next_nonce(AccountID) ->
%   case request(["/v2/accounts/", AccountID, "/next-nonce"]) of
%       {ok, #{"next_nonce" := Nonce}}           -> {ok, Nonce};
%       {ok, #{"reason" := "Account not found"}} -> {ok, 1};
%       {ok, #{"reason" := Reason}}              -> {error, Reason};
%       Error                                    -> Error
%   end.
    case request(["/v2/accounts/", AccountID]) of
        {ok, #{"nonce"  := Nonce}}               -> {ok, Nonce + 1};
        {ok, #{"reason" := "Account not found"}} -> {ok, 1};
        {ok, #{"reason" := Reason}}              -> {error, Reason};
        Error                                    -> Error
    end.


-spec dry_run(TX) -> {ok, Result} | {error, Reason}
    when TX     :: binary() | string(),
         Result :: term(),  % FIXME
         Reason :: term().  % FIXME
%% @doc
%% Execute a read-only transaction on the chain at the current height.
%% Equivalent of
%% ```
%% {ok, Hash} = vanillae:kb_current_hash(),
%% vanilla:dry_run(TX, Hash),
%% '''
%% NOTE:
%%  For this function to work the Aeternity node you are sending the request
%%  to must have its configuration set to `http: endpoints: dry-run: true'

dry_run(TX) ->
    dry_run(TX, []).


-spec dry_run(TX, Accounts) -> {ok, Result} | {error, Reason}
    when TX       :: binary() | string(),
         Accounts :: [pubkey()],
         Result   :: term(),  % FIXME
         Reason   :: term().  % FIXME

dry_run(TX, Accounts) ->
    case kb_current_hash() of
        {ok, Hash} -> dry_run(TX, Accounts, Hash);
        Error      -> Error
    end.


-spec dry_run(TX, Accounts, KBHash) -> {ok, Result} | {error, Reason}
    when TX       :: binary() | string(),
         Accounts :: [pubkey()],
         KBHash   :: binary() | string(),
         Result   :: term(),  % FIXME
         Reason   :: term().  % FIXME
%% @doc
%% Execute a read-only transaction on the chain at the height indicated by the
%% hash provided.

dry_run(TX, Accounts, KBHash) ->
    KBB = to_binary(KBHash),
    TXB = to_binary(TX),
    DryData = #{top       => KBB,
                accounts  => Accounts,
                txs       => [#{tx => TXB}],
                tx_events => true},
    JSON = zj:binary_encode(DryData),
    request("/v2/dry-run", JSON).

to_binary(S) when is_binary(S) -> S;
to_binary(S) when is_list(S)   -> list_to_binary(S).


-spec tx(ID) -> {ok, TX} | {error, Reason}
    when ID     :: tx_hash(),
         TX     :: transaction(),
         Reason :: ae_error() | string().
%% @doc
%% Retrieve a transaction by ID.

tx(ID) ->
    request(["/v2/transactions/", ID]).


-spec tx_info(ID) -> {ok, Info} | {error, Reason}
    when ID     :: tx_hash(),
         Info   :: term(), % FIXME
         Reason :: ae_error() | string().
%% @doc
%% Retrieve TX metadata by ID.

tx_info(ID) ->
    result(request(["/v2/transactions/", ID, "/info"])).


-spec post_tx(Data) -> {ok, Result} | {error, Reason}
    when Data   :: term(), % FIXME
         Result :: term(), % FIXME
         Reason :: ae_error() | string().
%% @doc
%% Post a transaction to the chain.

post_tx(Data) ->
    JSON = zj:binary_encode(#{tx => Data}),
    request("/v2/transactions", JSON).


-spec contract(ID) -> {ok, ContractData} | {error, Reason}
    when ID           :: contract_id(),
         ContractData :: contract_data(),
         Reason       :: ae_error() | string().
%% @doc
%% Retrieve a contract's metadata by ID.

contract(ID) ->
    result(request(["/v2/contracts/", ID])).


-spec contract_code(ID) -> {ok, Bytecode} | {error, Reason}
    when ID       :: contract_id(),
         Bytecode :: contract_byte_array(),
         Reason   :: ae_error() | string().

contract_code(ID) ->
    case request(["/v2/contracts/", ID, "/code"]) of
        {ok, #{"bytecode" := Bytecode}} -> {ok, Bytecode};
        {ok, #{"reason"   := Reason}}   -> {error, Reason};
        Error                           -> Error
    end.


-spec contract_poi(ID) -> {ok, Bytecode} | {error, Reason}
    when ID       :: contract_id(),
         Bytecode :: contract_byte_array(),
         Reason   :: ae_error() | string().

contract_poi(ID) ->
    request(["/v2/contracts/", ID, "/poi"]).

% TODO
%oracle(ID) ->
%    request(["/v2/oracles/", ID]).

% TODO
%oracle_queries(ID) ->
%    request(["/v2/oracles/", ID, "/queries"]).

% TODO
%oracle_queries_by_id(OracleID, QueryID) ->
%    request(["/v2/oracles/", OracleID, "/queries/", QueryID]).


-spec name(Name) -> {ok, Info} | {error, Reason}
    when Name   :: string(), % _ ++ ".chain"
         Info   :: name_info(),
         Reason :: ae_error() | string().
%% @doc
%% Retrieve a name's chain information.

name(Name) ->
    result(request(["/v2/names/", Name])).


% TODO
%channel(ID) ->
%    request(["/v2/channels/", ID]).


% FIXME: This should take a specific peer address:port otherwise it will be pointlessly
%        random.
-spec peer_pubkey() -> {ok, Pubkey} | {error, Reason}
    when Pubkey  :: peer_pubkey(),
         Reason  :: term(). % FIXME
%% @doc
%% Returns the given node's public key, assuming there an AE node is reachable at
%% the given address.

peer_pubkey() ->
    case request("/v2/peers/pubkey") of
        {ok, #{"pubkey" := Pubkey}} -> {ok, Pubkey};
        {ok, #{"reason" := Reason}} -> {error, Reason};
        Error                       -> Error
    end.


% TODO: Make a status/1 that allows the caller to query a specific node rather than
%       a random one from the pool.
-spec status() -> {ok, Status} | {error, Reason}
    when Status :: status(),
         Reason :: ae_error().
%% @doc
%% Retrieve the node's status and meta it currently has about the chain.

status() ->
    request("/v2/status").


-spec status_chainends() -> {ok, ChainEnds} | {error, Reason}
    when ChainEnds :: [keyblock_hash()],
         Reason    :: ae_error().
%% @doc
%% Retrieve the latest keyblock hashes

status_chainends() ->
    request("/v2/status/chain-ends").


request(Path) ->
    vanillae_man:request(unicode:characters_to_list(Path)).


request(Path, Payload) ->
    vanillae_man:request(unicode:characters_to_list(Path), Payload).


result({ok, #{"reason" := Reason}}) -> {error, Reason};
result(Received)                    -> Received.



%%% Contract calls

-spec contract_create(CreatorID, Path, InitArgs) -> Result
    when CreatorID :: unicode:chardata(),
         Path      :: file:filename(),
         InitArgs  :: [string()],
         Result    :: {ok, CreateTX} | {error, Reason},
         CreateTX  :: binary(),
         Reason    :: file:posix() | term().
%% @doc
%% This function reads the source of a Sophia contract (an .aes file)
%% and returns the unsigned create contract call data with default values.
%% For more control over exactly what those values are, use create_contract/8.

contract_create(CreatorID, Path, InitArgs) ->
    case next_nonce(CreatorID) of
        {ok, Nonce} ->
            Amount = 0,
            Gas = 100000,
            GasPrice = min_gas_price(),
            Fee = min_fee(),
            contract_create(CreatorID, Nonce,
                            Amount, Gas, GasPrice, Fee,
                            Path, InitArgs);
        Error ->
            Error
    end.


-spec contract_create(CreatorID, Nonce,
                      Amount, Gas, GasPrice, Fee,
                      Path, InitArgs) -> Result
    when CreatorID :: unicode:chardata(),
         Nonce     :: pos_integer(),
         Amount    :: non_neg_integer(),
         Gas       :: pos_integer(),
         GasPrice  :: pos_integer(),
         Fee       :: non_neg_integer(),
         Path      :: file:filename(),
         InitArgs  :: [string()],
         Result    :: {ok, CreateTX} | {error, Reason},
         CreateTX  :: binary(),
         Reason    :: term().
%% @doc
%% Create a "create contract" call using the supplied values.
%%
%% Contract creation is an even more opaque process than contract calls if you're new
%% to Aeternity.
%%
%% The meaning of each argument is as follows:
%% <ul>
%%  <li>
%%   <b>CreatorID:</b>
%%   This is the <em>public</em> key of the entity who will be posting the contract
%%   to the chain.
%%   The key must be encoded as a binary string prefixed with <<"ak_">>.
%%   The returned call will still need to be signed by the caller's <em>private</em>
%%   key.
%%  </li>
%%  <li>
%%   <b>Nonce:</b>
%%   This is a sequential integer value that ensures that the hash value of two
%%   sequential signed calls with the same contract ID, function and arguments can
%%   never be the same.
%%   This avoids replay attacks and ensures indempotency despite the distributed
%%   nature of the blockchain network).
%%   Every CallerID on the chain has a "next nonce" value that can be discovered by
%%   querying your Aeternity node (via `vanillae:next_nonce(CallerID)', for example).
%%  </li>
%%  <li>
%%   <b>Amount:</b>
%%   All Aeternity transactions can carry an "amount" spent from the origin account
%%   (in this case the `CallerID') to the destination. In a "Spend" transaction this
%%   is the only value that really matters, but in a contract call the utility is
%%   quite different, as you can pay money <em>into</em> a contract and have that
%%   contract hold it (for future payouts, to be held in escrow, as proof of intent
%%   to purchase or engage in an auction, whatever). Typically this value is 0, but
%%   of course there are very good reasons why it should be set to a non-zero value
%%   in the case of calls related to contract-governed payment systems.
%%  </li>
%%  <li>
%%   <b>Gas:</b>
%%   This number sets a limit on the maximum amount of computation the caller is willing
%%   to pay for on the chain.
%%   Both storage and thunks are costly as the entire Aeternity network must execute,
%%   verify, store and replicate all state changes to the chain.
%%   Each byte stored on the chain carries a cost of 20 gas, which is not an issue if
%%   you are storing persistent values of some state trasforming computation, but
%%   high enough to discourage frivolous storage of media on the chain (which would be
%%   a burden to the entire network).
%%   Computation is less expensive, but still costs and is calculated very similarly
%%   to the Erlang runtime's per-process reduction budget.
%%   The maximum amount of gas that a microblock is permitted to carry (its maximum
%%   computational weight, so to speak) is 6,000,000.
%%   Typical contract calls range between about 100 to 15,000 gas, so the default gas
%%   limit set by the `contract_call/6' function is only 20,000.
%%   Setting the gas limit to 6,000,000 or more will cause your contract call to fail.
%%   All transactions cost some gas with the exception of stateless or read-only
%%   calls to your Aeternity node (executed as "dry run" calls and not propagated to
%%   the network).
%%   The gas consumed by the contract call transaction is multiplied by the `GasPrice'
%%   provided and rolled into the block reward paid out to the node that mines the
%%   transaction into a microblock.
%%   Unused gas is refunded to the caller.
%%  </li>
%%  <li>
%%   <b>GasPrice:</b>
%%   This is a factor that is used calculate a value in aettos (the smallest unit of
%%   Aeternity's currency value) for the gas consumed. In times of high contention
%%   in the mempool increasing the gas price increases the value of mining a given
%%   transaction, thus making miners more likely to prioritize the high value ones.
%%  </li>
%%  <li>
%%   <b>Fee:</b>
%%   This value should really be caled `Bribe' or `Tip'.
%%   This is a flat fee in aettos that is paid into the block reward, thereby allowing
%%   an additional way to prioritize a given transaction above others, even if the
%%   transaction will not consume much gas.
%%  </li>
%%  <li>
%%   <b>ACI:</b>
%%   This is the compiled contract's metadata. It provides the information necessary
%%   for the contract call data to be formed in a way that the Aeternity runtime will
%%   understand.
%%   This ACI data must be already formatted in the native Erlang format as an .aci
%%   file rather than as the JSON serialized format produced by the Sophia CLI tool.
%%   The easiest way to create native ACI data is to use the Aeternity Launcher,
%%   a GUI tool with a "Developers' Workbench" feature that can assist with this.
%%  </li>
%%  <li>
%%   <b>ConID:</b>
%%   This is the on-chain address of the contract instance that is to be called.
%%   Note, this is different from the `name' of the contract, as a single contract may
%%   be deployed multiple times.
%%  </li>
%%  <li>
%%   <b>Fun:</b>
%%   This is the name of the entrypoint function to be called on the contract,
%%   provided as a string (not a binary string, but a textual string as a list).
%%  </li>
%%  <li>
%%   <b>Args:</b>
%%   This is a list of the arguments to provide to the function, listed in order
%%   according to the function's spec, and represented as strings (that is, an integer
%%   argument of `10' must be cast to the textual representation `"10"').
%%  </li>
%% '''
%% As should be obvious from the above description, it is pretty helpful to have a
%% source copy of the contract you intend to call so that you can re-generate the ACI
%% if you do not already have a copy, and can check the spec of a function before
%% trying to form a contract call.

contract_create(CreatorID, Nonce,
                Amount, Gas, GasPrice, Fee,
                Path, InitArgs) ->
    case aeso_compiler:file(Path, [{aci, json}]) of
        {ok, Compiled} ->
            contract_create2(CreatorID, Nonce,
                             Amount, Gas, GasPrice, Fee,
                             Compiled, InitArgs);
        Error ->
            Error
    end.

contract_create2(CreatorID, Nonce,
                 Amount, Gas, GasPrice, Fee,
                 Compiled, InitArgs) ->
    AACI = prepare_aaci(maps:get(aci, Compiled)),
    case encode_call_data(AACI, "init", InitArgs) of
        {ok, CallData} ->
            contract_create3(CreatorID, Nonce,
                             Amount, Gas, GasPrice, Fee,
                             Compiled, CallData);
        Error ->
            Error
    end.

contract_create3(CreatorID, Nonce,
                 Amount, Gas, GasPrice, Fee,
                 Compiled, CallData) ->
    PK = unicode:characters_to_binary(CreatorID),
    try
        {account_pubkey, OwnerID} = aeser_api_encoder:decode(PK),
        contract_create4(OwnerID, Nonce,
                         Amount, Gas, GasPrice, Fee,
                         Compiled, CallData)
    catch
        Error:Reason -> {Error, Reason}
    end.

contract_create4(OwnerID, Nonce,
                 Amount, Gas, GasPrice, Fee,
                 Compiled, CallData) ->
    Code = aeser_contract_code:serialize(Compiled),
    VM = 7,
    ABI = 3,
    <<CTVersion:32>> = <<VM:16, ABI:16>>,
    ContractCreateVersion = 1,
    TTL = 0,
    Type = contract_create_tx,
    Fields =
        [{owner_id,   aeser_id:create(account, OwnerID)},
         {nonce,      Nonce},
         {code,       Code},
         {ct_version, CTVersion},
         {fee,        Fee},
         {ttl,        TTL},
         {deposit,    0},
         {amount,     Amount},
         {gas,        Gas},
         {gas_price,  GasPrice},
         {call_data,  CallData}],
    Template =
        [{owner_id,   id},
         {nonce,      int},
         {code,       binary},
         {ct_version, int},
         {fee,        int},
         {ttl,        int},
         {deposit,    int},
         {amount,     int},
         {gas,        int},
         {gas_price,  int},
         {call_data,  binary}],
    TXB = aeser_chain_objects:serialize(Type, ContractCreateVersion, Template, Fields),
    try
        {ok, aeser_api_encoder:encode(transaction, TXB)}
    catch
        error:Reason -> {error, Reason}
    end.


-spec read_aci(Path) -> Result
    when Path   :: file:filename(),
         Result :: {ok, ACI} | {error, Reason},
         ACI    :: tuple(), % FIXME: Change to correct Sophia record
         Reason :: file:posix() | bad_aci.
%% @doc
%% This function reads the contents of an .aci file produced by AEL (the Aeternity
%% Launcher). ACI data is required for the contract call encoder to function properly.
%% ACI data is can be generated and stored in JSON data, and the Sophia CLI tool
%% can perform this action. Unfortunately, JSON is not the way that ACI data is
%% represented internally, and here we need the actual native representation. For
%% that reason Aeternity's GUI launcher (AEL) has a "Developer's Workbench" tool
%% that can produce an .aci file from a contract's source code and store it in the
%% native Erlang format.
%%
%% ACI encding/decoding and contract call encoding is significantly complex enough that
%% this provides for a pretty large savings in complexity for this library, dramatically
%% reduces runtime dependencies, and makes call encoding much more efficient (as a
%% huge number of steps are completely eliminated by this).

read_aci(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            case zx_lib:b_to_ts(Bin) of
                error -> {error, bad_aci};
                OK    -> OK
            end;
        Error ->
            Error
    end.


-spec contract_call(CallerID, AACI, ConID, Fun, Args) -> Result
    when CallerID :: unicode:chardata(),
         AACI     :: map(),
         ConID    :: unicode:chardata(),
         Fun      :: string(),
         Args     :: [string()],
         Result   :: {ok, CallTX} | {error, Reason},
         CallTX   :: binary(),
         Reason   :: term().
%% @doc
%% Form a contract call using hardcoded default values for `Gas', `GasPrice', `Fee',
%% and `Amount' to simplify the call (10 args is a bit much for normal calls!).
%% The values used are 20k for `Gas' and `Fee', the `GasPrice' is fixed at 1b (the
%% default "miner minimum" defined in default configs), and the `Amount' is 0.
%%
%% For details on the meaning of these and other argument values see the doc comment
%% for contract_call/10.

contract_call(CallerID, AACI, ConID, Fun, Args) ->
    {ok, Nonce} = next_nonce(CallerID),
    Gas = min_gas(),
    GasPrice = min_gas_price(),
    Fee = min_fee(),
    Amount = 0,
    contract_call(CallerID, Nonce,
                  Gas, GasPrice, Fee, Amount,
                  AACI, ConID, Fun, Args).


-spec contract_call(CallerID, Gas, AACI, ConID, Fun, Args) -> Result
    when CallerID :: unicode:chardata(),
         Gas      :: pos_integer(),
         AACI     :: map(),
         ConID    :: unicode:chardata(),
         Fun      :: string(),
         Args     :: [string()],
         Result   :: {ok, CallTX} | {error, Reason},
         CallTX   :: binary(),
         Reason   :: term().
%% @doc
%% Just like contract_call/5, but allows you to specify the amount of gas
%% without getting into a major adventure with the other arguments.
%%
%% For details on the meaning of these and other argument values see the doc comment
%% for contract_call/10.

contract_call(CallerID, Gas, AACI, ConID, Fun, Args) ->
    {ok, Nonce} = next_nonce(CallerID),
    GasPrice = min_gas_price(),
    Fee = min_fee(),
    Amount = 0,
    contract_call(CallerID, Nonce,
                  Gas, GasPrice, Fee, Amount,
                  AACI, ConID, Fun, Args).


-spec contract_call(CallerID, Nonce,
                    Gas, GasPrice, Fee, Amount,
                    AACI, ConID, Fun, Args) -> Result
    when CallerID :: unicode:chardata(),
         Nonce    :: pos_integer(),
         Gas      :: pos_integer(),
         GasPrice :: pos_integer(),
         Fee      :: non_neg_integer(),
         Amount   :: non_neg_integer(),
         AACI     :: map(),
         ConID    :: unicode:chardata(),
         Fun      :: string(),
         Args     :: [string()],
         Result   :: {ok, CallTX} | {error, Reason},
         CallTX   :: binary(),
         Reason   :: term().
%% @doc
%% Form a contract call using the supplied values.
%%
%% Contract call formation is a rather opaque process if you're new to Aeternity or
%% smart contract execution in general.
%%
%% The meaning of each argument is as follows:
%% <ul>
%%  <li>
%%   <b>CallerID:</b>
%%   This is the <em>public</em> key of the entity making the contract call.
%%   The key must be encoded as a binary string prefixed with <<"ak_">>.
%%   The returned call will still need to be signed by the caller's <em>private</em>
%%   key.
%%  </li>
%%  <li>
%%   <b>Nonce:</b>
%%   This is a sequential integer value that ensures that the hash value of two
%%   sequential signed calls with the same contract ID, function and arguments can
%%   never be the same.
%%   This avoids replay attacks and ensures indempotency despite the distributed
%%   nature of the blockchain network).
%%   Every CallerID on the chain has a "next nonce" value that can be discovered by
%%   querying your Aeternity node (via `vanillae:next_nonce(CallerID)', for example).
%%  </li>
%%  <li>
%%   <b>Gas:</b>
%%   This number sets a limit on the maximum amount of computation the caller is willing
%%   to pay for on the chain.
%%   Both storage and thunks are costly as the entire Aeternity network must execute,
%%   verify, store and replicate all state changes to the chain.
%%   Each byte stored on the chain carries a cost of 20 gas, which is not an issue if
%%   you are storing persistent values of some state trasforming computation, but
%%   high enough to discourage frivolous storage of media on the chain (which would be
%%   a burden to the entire network).
%%   Computation is less expensive, but still costs and is calculated very similarly
%%   to the Erlang runtime's per-process reduction budget.
%%   The maximum amount of gas that a microblock is permitted to carry (its maximum
%%   computational weight, so to speak) is 6,000,000.
%%   Typical contract calls range between about 100 to 15,000 gas, so the default gas
%%   limit set by the `contract_call/6' function is only 20,000.
%%   Setting the gas limit to 6,000,000 or more will cause your contract call to fail.
%%   All transactions cost some gas with the exception of stateless or read-only
%%   calls to your Aeternity node (executed as "dry run" calls and not propagated to
%%   the network).
%%   The gas consumed by the contract call transaction is multiplied by the `GasPrice'
%%   provided and rolled into the block reward paid out to the node that mines the
%%   transaction into a microblock.
%%   Unused gas is refunded to the caller.
%%  </li>
%%  <li>
%%   <b>GasPrice:</b>
%%   This is a factor that is used calculate a value in aettos (the smallest unit of
%%   Aeternity's currency value) for the gas consumed. In times of high contention
%%   in the mempool increasing the gas price increases the value of mining a given
%%   transaction, thus making miners more likely to prioritize the high value ones.
%%  </li>
%%  <li>
%%   <b>Fee:</b>
%%   This value should really be caled `Bribe' or `Tip'.
%%   This is a flat fee in aettos that is paid into the block reward, thereby allowing
%%   an additional way to prioritize a given transaction above others, even if the
%%   transaction will not consume much gas.
%%  </li>
%%  <li>
%%   <b>Amount:</b>
%%   All Aeternity transactions can carry an "amount" spent from the origin account
%%   (in this case the `CallerID') to the destination. In a "Spend" transaction this
%%   is the only value that really matters, but in a contract call the utility is
%%   quite different, as you can pay money <em>into</em> a contract and have that
%%   contract hold it (for future payouts, to be held in escrow, as proof of intent
%%   to purchase or engage in an auction, whatever). Typically this value is 0, but
%%   of course there are very good reasons why it should be set to a non-zero value
%%   in the case of calls related to contract-governed payment systems.
%%  </li>
%%  <li>
%%   <b>ACI:</b>
%%   This is the compiled contract's metadata. It provides the information necessary
%%   for the contract call data to be formed in a way that the Aeternity runtime will
%%   understand.
%%   This ACI data must be already formatted in the native Erlang format as an .aci
%%   file rather than as the JSON serialized format produced by the Sophia CLI tool.
%%   The easiest way to create native ACI data is to use the Aeternity Launcher,
%%   a GUI tool with a "Developers' Workbench" feature that can assist with this.
%%  </li>
%%  <li>
%%   <b>ConID:</b>
%%   This is the on-chain address of the contract instance that is to be called.
%%   Note, this is different from the `name' of the contract, as a single contract may
%%   be deployed multiple times.
%%  </li>
%%  <li>
%%   <b>Fun:</b>
%%   This is the name of the entrypoint function to be called on the contract,
%%   provided as a string (not a binary string, but a textual string as a list).
%%  </li>
%%  <li>
%%   <b>Args:</b>
%%   This is a list of the arguments to provide to the function, listed in order
%%   according to the function's spec, and represented as strings (that is, an integer
%%   argument of `10' must be cast to the textual representation `"10"').
%%  </li>
%% '''
%% As should be obvious from the above description, it is pretty helpful to have a
%% source copy of the contract you intend to call so that you can re-generate the ACI
%% if you do not already have a copy, and can check the spec of a function before
%% trying to form a contract call.

contract_call(CallerID, Nonce, Gas, GP, Fee, Amount, AACI, ConID, Fun, Args) ->
    case encode_call_data(AACI, Fun, Args) of
        {ok, CD} -> contract_call2(CallerID, Nonce, Gas, GP, Fee, Amount, ConID, CD);
        Error    -> Error
    end.

contract_call2(CallerID, Nonce, Gas, GasPrice, Fee, Amount, ConID, CallData) ->
    CallerBin = unicode:characters_to_binary(CallerID),
    try
        {account_pubkey, PK}  = aeser_api_encoder:decode(CallerBin),
        contract_call3(PK, Nonce, Gas, GasPrice, Fee, Amount, ConID, CallData)
    catch
        Error:Reason -> {Error, Reason}
    end.

contract_call3(PK, Nonce, Gas, GasPrice, Fee, Amount, ConID, CallData) ->
    ConBin = unicode:characters_to_binary(ConID),
    try
        {contract_pubkey, CK} = aeser_api_encoder:decode(ConBin),
        contract_call4(PK, Nonce, Gas, GasPrice, Fee, Amount, CK, CallData)
    catch
        Error:Reason -> {Error, Reason}
    end.

contract_call4(PK, Nonce, Gas, GasPrice, Fee, Amount, CK, CallData) ->
    ABI = 3,
    TTL = 0,
    CallVersion = 1,
    Type = contract_call_tx,
    Fields =
        [{caller_id,   aeser_id:create(account, PK)},
         {nonce,       Nonce},
         {contract_id, aeser_id:create(contract, CK)},
         {abi_version, ABI},
         {fee,         Fee},
         {ttl,         TTL},
         {amount,      Amount},
         {gas,         Gas},
         {gas_price,   GasPrice},
         {call_data,   CallData}],
    Template =
        [{caller_id,   id},
         {nonce,       int},
         {contract_id, id},
         {abi_version, int},
         {fee,         int},
         {ttl,         int},
         {amount,      int},
         {gas,         int},
         {gas_price,   int},
         {call_data,   binary}],
    TXB = aeser_chain_objects:serialize(Type, CallVersion, Template, Fields),
    try
        {ok, aeser_api_encoder:encode(transaction, TXB)}
    catch
        error:Reason -> {error, Reason}
    end.


-spec prepare_contract(File) -> {ok, AACI} | {error, Reason}
    when File   :: file:filename(),
         AACI   :: map(),
         Reason :: term().
%% @doc
%% Compile a contract and extract the function spec meta for use in future formation
%% of calldata

prepare_contract(File) ->
    case aeso_compiler:file(File, [{aci, json}]) of
        {ok, #{aci := ACI}} -> {ok, prepare_aaci(ACI)};
        Error               -> Error
    end.

prepare_aaci(ACI) ->
    [{NameBin, SpecDefs}] =
        [{N, F}
         || #{contract := #{kind      := contract_main,
                            functions := F,
                            name      := N}} <- ACI],
    Name = binary_to_list(NameBin),
    Specs = lists:foldl(fun simplify_specs/2, #{}, SpecDefs),
    {aaci, Name, Specs}.

simplify_specs(#{name := NameBin, arguments := ArgDefs}, Specs) ->
    Name = binary_to_list(NameBin),
    ArgTypes = lists:map(fun simplify_args/1, ArgDefs),
    maps:put(Name, ArgTypes, Specs).

simplify_args(#{name := NameBin, type := TypeBin}) ->
    Name = binary_to_list(NameBin),
    Type = type(TypeBin),
    {Name, Type}.

type(<<"int">>)             -> integer;
type(<<"address">>)         -> address;
type(<<"contract">>)        -> contract;
type(<<"bool">>)            -> boolean;
type(Name)                  -> binary_to_list(Name).
%type(#{<<"list">> := T})    -> {list, type(T)};
%type(#{<<"tuple">> := T})   -> {tuple, type(T)};
%type(#{<<"map">> := {K, V}} -> {map, type(K), type(V)};
%type(<<"string">>)          -> string;

coerce({{ArgName, integer},  S}, {Good, Broken}) ->
    try
        N = list_to_integer(S),
        {[N | Good], Broken}
    catch
        error:Reason -> {Good, [{ArgName, Reason} | Broken]}
    end;
coerce({{ArgName, address},  S}, {Good, Broken}) ->
    try
        case aeser_api_encoder:decode(unicode:characters_to_binary(S)) of
            {account_pubkey, Key} -> {[{address, Key} | Good], Broken};
            _                     -> {Good, [{ArgName, bad_pubkey} | Broken]}
        end
    catch
        error:Reason -> {Good, [{ArgName, Reason} | Broken]}
    end;
coerce({{ArgName, contract}, S}, {Good, Broken}) ->
    try
        case aeser_api_encoder:decode(unicode:characters_to_binary(S)) of
            R = {contract_bytearray, _} -> {[R | Good], Broken};
            _                           -> {Good, [{ArgName, bad_contract} | Broken]}
        end
    catch
        error:Reason -> {Good, [{ArgName, Reason} | Broken]}
    end;
coerce({{_, bool}, true}, {Good, Broken}) ->
    {[true | Good], Broken};
coerce({{_, bool}, false}, {Good, Broken}) ->
    {[false | Good], Broken};
coerce({{ArgName, bool},  _}, {Good, Broken}) ->
    {Good, [{ArgName, not_bool} | Broken]};
coerce({_, S}, {Good, Broken}) ->
    {[S | Good], Broken}.


-spec min_gas_price() -> integer().
%% @doc
%% This function always returns 1,000,000,000 in the current version.
%%
%% This is the minimum gas price returned by aec_tx_pool:minimum_miner_gas_price(),
%% (the default set in aeternity_config_schema.json).
%%
%% Surely there can be some more nuance to this, but until a "gas station" type
%% market/chain survey service exists we will use this naive value as a default
%% and users can call contract_call/10 if they want more fine-tuned control over the
%% price. This won't really matter much until the chain has a high enough TPS that
%% contention becomes an issue.

min_gas_price() ->
    1000000000.


-spec min_gas() -> integer().
%% @doc
%% This function always returns 20,000 in the current version.
%%
%% There is no actual minimum gas price, but this figure provides a lower limit toward
%% successful completion of general contract calls while not too severely limiting the
%% number of TXs that may appear in a single microblock based on the per-block gas
%% maximum (6,000,000 / 20,000 = 300 TXs in a microblock -- which at the moment seems
%% like plenty).

min_gas() ->
    20000.


-spec min_fee() -> integer().
%% @doc
%% This function always returns 200,000,000,000,000 in the current version.
%%
%% This is the minimum fee amount currently accepted -- it is up to callers whether
%% they want to customize this value higher (or possibly lower, though as things stand
%% that would only work on an independent AE-based network, not the actual Aeternity
%% mainnet or testnet).

min_fee() ->
    200000000000000.


encode_call_data({aaci, _, FunDefs}, Fun, Args) ->
    case maps:find(Fun, FunDefs) of
        {ok, ArgDef} -> encode_call_data2(ArgDef, Fun, Args);
        error        -> {error, bad_fun_name}
    end.

encode_call_data2(ArgDef, Fun, Args) ->
    DefLength = length(ArgDef),
    ArgLength = length(Args),
    if
        DefLength =:= ArgLength -> encode_call_data3(ArgDef, Fun, Args);
        DefLength >   ArgLength -> {error, too_few_args};
        DefLength   < ArgLength -> {error, too_many_args}
    end.

encode_call_data3(ArgDef, Fun, Args) ->
    Binding = lists:zip(ArgDef, Args),
    case lists:foldl(fun coerce/2, {[], []}, Binding) of
        {Coerced, []} ->
            Reversed = lists:reverse(Coerced),
            aeb_fate_abi:create_calldata(Fun, Reversed);
        {_, Errors} ->
            {error, {args, lists:reverse(Errors)}}
    end.


verify_signature(Sig, Message, PubKey) ->
    case aeser_api_encoder:decode(PubKey) of
        {account_pubkey, PK} -> verify_signature2(Sig, Message, PK);
        Other                -> {error, {bad_key, Other}}
    end.

verify_signature2(Sig, Message, PK) ->
    Prefix = <<"aeternity Signed Message:\n">>,
    {ok, PSize} = vencode(byte_size(Prefix)),
    {ok, MSize} = vencode(byte_size(Message)),
    Smashed = iolist_to_binary([PSize, Prefix, MSize, Message]),
    {ok, Hashed} = eblake2:blake2b(32, Smashed),
    Signature = <<(binary_to_integer(Sig, 16)):(64 * 8)>>,
    Result = enacl:sign_verify_detached(Signature, Hashed, PK),
    {ok, Result}.


vencode(N) when N < 0 ->
    {error, {negative_N, N}};
vencode(N) when N < 16#FD ->
    {ok, <<N>>};
vencode(N) when N =< 16#FFFF ->
    NBytes = eu(N, 2),
    {ok, <<16#FD, NBytes/binary>>};
vencode(N) when N =< 16#FFFF_FFFF ->
    NBytes = eu(N, 4),
    {ok, <<16#FE, NBytes/binary>>};
vencode(N) when N < (2 bsl 64) ->
    NBytes = eu(N, 8),
    {ok, <<16#FF, NBytes/binary>>}.

eu(N, Size) ->
    Bytes = binary:encode_unsigned(N, little),
    NExtraZeros = Size - byte_size(Bytes),
    ExtraZeros = << <<0>> || _ <- lists:seq(1, NExtraZeros) >>,
    <<Bytes/binary, ExtraZeros/binary>>.


%%% Debug functionality

% debug_network() ->
%     request("/v2/debug/network").
%
% /v2/debug/contracts/create
% /v2/debug/contracts/call
% /v2/debug/oracles/register
% /v2/debug/oracles/extend
% /v2/debug/oracles/query
% /v2/debug/oracles/respond
% /v2/debug/names/preclaim
% /v2/debug/names/claim
% /v2/debug/names/update
% /v2/debug/names/transfer
% /v2/debug/names/revoke
% /v2/debug/transactions/spend
% /v2/debug/channels/create
% /v2/debug/channels/deposit
% /v2/debug/channels/withdraw
% /v2/debug/channels/snapshot/solo
% /v2/debug/channels/set-delegates
% /v2/debug/channels/close/mutual
% /v2/debug/channels/close/solo
% /v2/debug/channels/slash
% /v2/debug/channels/settle
% /v2/debug/transactions/pending
% /v2/debug/names/commitment-id
% /v2/debug/accounts/beneficiary
% /v2/debug/accounts/node
% /v2/debug/peers
% /v2/debug/transactions/dry-run
% /v2/debug/transactions/paying-for
% /v2/debug/check-tx/pool/{hash}
% /v2/debug/token-supply/height/{height}
% /v2/debug/crash


-spec start() -> ok | {error, Reason :: term()}.

start() ->
    application:start(vanillae).


-spec stop() -> ok | {error, Reason :: term()}.

stop() ->
    application:stop(vanillae).


-spec start(normal, term()) -> {ok, pid()}.

start(normal, _Args) ->
    vanillae_sup:start_link().


-spec stop(term()) -> ok.

stop(_State) ->
    ok.
