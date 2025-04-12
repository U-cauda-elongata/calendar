export type JsonObject = { [Key in string]?: JsonValue };
export type JsonArray = JsonValue[];

/**
Matches any valid JSON value.
Source: https://github.com/sindresorhus/type-fest/blob/master/source/basic.d.ts
*/
export type JsonValue =
  | string
  | number
  | boolean
  | null
  | JsonObject
  | JsonArray;

export interface ElmApp {
  ports: {
    interopFromElm: PortFromElm<FromElm>;
    interopToElm: PortToElm<ToElm>;
    [key: string]: UnknownPort;
  };
}

export type FromElm = { data : string; tag : "slideViewportInto" } | { data : string; tag : "showModal" } | { data : { title : string; url? : string }; tag : "share" } | { data : string; tag : "setLang" } | { tag : "removeScrollEventListener" } | { data : string; tag : "preventScrollFocus" } | { data : string; tag : "copy" } | { data : string; tag : "close" };

export type ToElm = { tag : "OnScrollToBottom" };

export type Flags = ({ features : { copy : boolean; share : boolean } } & { languages : string[] } & { feeds : ({ id : string } & { title : string } & { lang : string } & { icon : { height : number; url : string; width : number } })[] } & { observances : JsonValue });

export namespace Main {
  function init(options: { node?: HTMLElement | null; flags: Flags }): ElmApp;
}

export as namespace Elm;

export { Elm };

export type UnknownPort = PortFromElm<unknown> | PortToElm<unknown> | undefined;

export type PortFromElm<Data> = {
  subscribe(callback: (fromElm: Data) => void): void;
  unsubscribe(callback: (fromElm: Data) => void): void;
};

export type PortToElm<Data> = { send(data: Data): void };
